module Import (
    importDebData,
    importTar
) where

import Store

import qualified Codec.Archive.Tar as Tar
import Codec.Archive.Tar.Entry
import Data.Bits
import qualified Data.ByteString.Lazy as B
import System.Exit
import System.IO
import System.Process

importTar :: Handle -> IO (Maybe StoreTag)
importTar h = do
    hSetBinaryMode h True
    tarball <- B.hGetContents h
    let emit entry rest = let path = fromTarPathToPosixPath $ entryTarPath entry in case entryContent entry of
            NormalFile bs _ -> (path, File ((entryPermissions entry .&. 0o111) /= 0) bs) : rest
            Directory -> rest
            SymbolicLink target -> (path, Symlink $ fromLinkTargetToPosixPath target) : rest
            HardLink target -> (path, Hardlink $ fromLinkTargetToPosixPath target) : rest
            _ -> error "unhandled tar entry type"
    tag <- importTag $ Tar.foldEntries emit [] error $ Tar.read tarball
    return $ Just tag

importDebData :: Handle -> IO (Maybe StoreTag)
importDebData h = do
    (Nothing, Just stdout, Nothing, p) <- createProcess (proc "dpkg-deb" ["--fsys-tarfile", "/dev/stdin"]) {
        std_in = UseHandle h,
        std_out = CreatePipe,
        close_fds = True
    }
    maybeTag <- importTar stdout
    code <- waitForProcess p
    if code == ExitSuccess then return maybeTag else return Nothing
