{-# LANGUAGE Rank2Types #-}
module Codec.Archive.LibArchive (
    readArchive,
    next,
    ArchiveEntry(),
    ArchiveEntryConsumer(..),
    module Codec.Archive.LibArchive.Internal.Entry
) where

import Codec.Archive.LibArchive.Internal
import Codec.Archive.LibArchive.Internal.Entry
import Control.Concurrent
import Control.Exception
import qualified Data.ByteString as B
import Data.ByteString.Unsafe
import Data.IORef
import Data.Iteratee
import Foreign.Ptr

instance NullPoint (Maybe a) where
    empty = Nothing

instance Nullable (Maybe a) where
    nullC Nothing = True
    nullC (Just _) = False

next :: Monad m => Iteratee (Maybe v) m v
next = liftI go
    where
    go s = case s of
        EOF _ -> throwRecoverableErr (setEOF s) go
        Chunk Nothing -> next
        Chunk (Just v) -> idone v (Chunk empty)

newtype ArchiveEntryConsumer = ArchiveEntryConsumer {
    runConsumer :: forall a. Iteratee B.ByteString IO a -> IO a
}

readArchive ::
    Iteratee (Maybe (ArchiveEntry, ArchiveEntryConsumer)) IO a ->
    IO (Iteratee B.ByteString IO a)
readArchive iter = do
    resultBox <- newEmptyMVar
    forkIO $ handle (putMVar resultBox . throwErr) $ do
        streamBox <- newEmptyMVar
        bufRef <- newIORef B.empty
        final <- bracket archiveReadNew archiveReadFinish $ \ archive -> do
            archiveReadSupportCompressionAll archive
            archiveReadSupportFormatAll archive
            archiveReadOpen archive (streamBox, bufRef, resultBox) noop reader noop
            bracket archiveEntryNew archiveEntryFree $ \ entry ->
                enumEof =<< readEntries archive entry iter
        putMVar resultBox final
    takeMVar resultBox
    where
    noop _ _ = return ArchiveOK
    reader _ (streamBox, bufRef, resultBox) = do
        putMVar resultBox $ liftI $ \ s -> joinIM $ do
            putMVar streamBox s
            takeMVar resultBox
        s <- takeMVar streamBox
        case s of
            EOF _ -> return (nullPtr, 0)
            Chunk buf -> do
                writeIORef bufRef buf -- save a reference that will last until the next read
                (p, l) <- unsafeUseAsCStringLen buf return
                return (castPtr p, fromIntegral l)

readEntries :: Archive -> ArchiveEntry ->
    Iteratee (Maybe (ArchiveEntry, ArchiveEntryConsumer)) IO a ->
    IO (Iteratee s IO a)
readEntries archive entry iter = runIter iter onDone onCont
    where
    onDone a s = idoneM a $! case s of
        EOF e -> EOF e
        Chunk _ -> EOF Nothing
    onCont k (Just e) = return $ throwErr e
    onCont k Nothing = do
        err <- archiveReadNextHeader2 archive entry
        case err of
            ArchiveOK -> readEntries archive entry $ k $ Chunk $ Just (entry, ArchiveEntryConsumer $ run . joinIM . readEntry archive 0)
            ArchiveEOF -> runIter (k $ EOF Nothing) onDone stopPlease
            _ -> do
                errstr <- archiveErrorString archive
                return $ throwErr $ toException $ enStrExc errstr

-- FIXME: offset should be Int64 or a C type, and we should feed sparse
-- gaps to the consumer in reasonably-sized chunks.
readEntry :: Archive -> Int -> Iteratee B.ByteString IO a -> IO (Iteratee (Maybe s) IO a)
readEntry archive lastoff = \ iter -> runIter iter onDone onCont
    where
    onDone _ (EOF (Just e)) = return $ throwErr e
    onDone a _ = idoneM a $ Chunk empty
    onCont k (Just e) = return $ throwErr e
    onCont k Nothing = do
        (err, buf, len, off) <- archiveReadDataBlock archive
        case err of
            ArchiveOK -> do
                let len' = fromIntegral len
                let off' = fromIntegral off
                bs <- B.packCStringLen (castPtr buf, len')
                let onCont' k' (Just e) = return $ throwErr e
                    onCont' k' Nothing = readEntry archive (off' + len') $ k' $ Chunk bs
                if off' == lastoff then onCont' k Nothing else runIter (k $ Chunk $ B.replicate (off' - lastoff) 0) onDone onCont'
            ArchiveEOF -> runIter (k $ EOF Nothing) onDone stopPlease
            _ -> do
                errstr <- archiveErrorString archive
                return $ throwErr $ toException $ enStrExc errstr

stopPlease :: Monad m => (Stream s -> Iteratee s m a) -> Maybe SomeException -> m (Iteratee s' m a')
stopPlease _ (Just e) = return $ throwErr e
stopPlease _ Nothing = return $ throwErr $ toException $ enStrExc "divergent in stopPlease" -- DivergentException
