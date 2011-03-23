module Import (
    importDebData,
    importTar
) where

import Store

import Codec.Archive.LibArchive
import Data.Bits
import qualified Data.ByteString.Lazy as B
import Data.Iteratee
import Data.Iteratee.IO
import System.Exit
import System.IO
import System.Process

importTar :: Handle -> IO StoreTag
importTar h = run $ joinIM $ enumHandle 16384 h $ joinIM $ readArchive $ joinIM $ mapMaybeI emit importTag
    where
    emit (entry, contents) = do
        path <- archiveEntryPathnameW entry
        filetype <- archiveEntryFiletype entry
        case filetype of
            ArchiveEntryTypeReg -> do
                mode <- archiveEntryMode entry
                size <- archiveEntrySize entry
                return $ Just (path, File ((mode .&. 0o111) /= 0) size (runConsumer contents))
            ArchiveEntryTypeNone -> do
                target <- archiveEntryHardlinkW entry
                return $ Just (path, Hardlink target)
            ArchiveEntryTypeLink -> do
                target <- archiveEntrySymlinkW entry
                return $ Just (path, Symlink target)
            ArchiveEntryTypeDir -> return Nothing
            _ -> fail $ "unhandled archive entry type " ++ show filetype

mapMaybeI :: Monad m => (a -> m (Maybe b)) -> Iteratee b m c -> m (Iteratee (Maybe a) m c)
mapMaybeI f iter = runIter iter onDone onCont
    where
    onDone _ (EOF (Just e)) = return $ throwErr e
    onDone a _ = idoneM a (Chunk empty)
    onCont _ (Just e) = return $ throwErr e
    onCont k Nothing = return $ liftI go
        where
        go s = case s of
            EOF maybeExc -> joinIM $ mapMaybeI f $ k $ EOF maybeExc
            Chunk Nothing -> liftI go
            Chunk (Just v) -> joinIM $ do
                mb <- f v
                case mb of
                    Nothing -> return $ liftI go
                    Just b -> mapMaybeI f $ k $ Chunk b

importDebData :: Handle -> IO (Maybe StoreTag)
importDebData h = do
    (Nothing, Just stdout, Nothing, p) <- createProcess (proc "dpkg-deb" ["--fsys-tarfile", "/dev/stdin"]) {
        std_in = UseHandle h,
        std_out = CreatePipe,
        close_fds = True
    }
    tag <- importTar stdout
    code <- waitForProcess p
    if code == ExitSuccess then return (Just tag) else return Nothing
