module Store (
    readTagFile,
    mergeTags,
    buildTag,
    prefixTag,
    extractTag,
    resolveTag,
    nameTag,
    StoreFile(..), importTag,
    StoreTag()
) where

import Directory

import Control.Concurrent
import Control.Monad
import qualified Data.ByteString.Lazy as B
import Data.Char
import Data.List
import Network.URI (escapeURIString)
import System.Directory
import System.Environment
import System.Exit
import System.FilePath.Posix
import System.IO
import System.IO.Unsafe
import System.Posix.User
import System.Process

newtype StoreTag = StoreTag String deriving Show

readTagFile :: StoreTag -> String -> String
readTagFile (StoreTag tag) path = cause ("can't read path " ++ show path ++ " from store tag " ++ tag) $
    run "git" ["cat-file", "blob", tag ++ ":" ++ path]

mergeTags :: [StoreTag] -> StoreTag
mergeTags tags = cause "merge" $ storeTag "./merge-trees" [ tag | StoreTag tag <- tags ]

buildTag :: StoreTag -> String -> StoreTag
buildTag (StoreTag root) cmd = unsafePerformIO $ withTemporaryDirectory "/tmp/apters-build" $ \ tmpdir -> do
    let indexfile = tmpdir </> "index"
    let worktree = tmpdir </> "build"
    environ <- liftM ([("GIT_INDEX_FILE", indexfile), ("GIT_WORK_TREE", worktree)] ++) getEnvironment
    let base cmd args = CreateProcess {
            cmdspec = RawCommand cmd args,
            cwd = Nothing,
            env = Just environ,
            std_in = CreatePipe,
            std_out = CreatePipe,
            std_err = CreatePipe,
            close_fds = True
        }
    let expectSilence cmd args = do
        (Just i, Just o, Just e, p) <- createProcess $ base cmd args
        hClose i
        o' <- hGetContents o
        e' <- hGetContents e
        all <- mergeIO o' e'
        ex <- waitForProcess p
        case (all, ex) of
            ("", ExitSuccess) -> return ()
            _ -> fail $ "running " ++ intercalate " " (cmd : args) ++ ": " ++ show ex ++ ": " ++ all

    createDirectory worktree
    expectSilence "git" ["read-tree", root]
    expectSilence "git" ["checkout-index", "-f", "-a"]

    uid <- getRealUserID
    gid <- getRealGroupID
    let chroot = ["/usr/sbin/chroot", "--userspec=" ++ show uid ++ ":" ++ show gid, worktree, cmd]
    (Just i1, Nothing, Nothing, p1) <- createProcess (base "/usr/bin/sudo" chroot) {
        env = Just [],
        std_out = Inherit,
        std_err = Inherit
    }
    hClose i1
    ex1 <- waitForProcess p1
    unless (ex1 == ExitSuccess) $ fail $ "running " ++ cmd ++ ": " ++ show ex1

    expectSilence "git" ["add", "-A", "-f", "."]

    (Just i2, Just o2, Just e2, p2) <- createProcess $ base "git" ["write-tree"]
    hClose i2
    o' <- hGetContents o2
    e' <- hGetContents e2
    ex <- waitForProcess p2
    case (lines o', e', ex) of
        ([l], "", ExitSuccess) | length l == 40 && all isHexDigit l -> return $ StoreTag l
        _ -> fail $ "running git write-tree: " ++ show ex ++ ": " ++ o' ++ e'

prefixTag :: String -> StoreTag -> StoreTag
prefixTag path (StoreTag tag) = cause ("prefix " ++ show path) $ storeTag "./prefix" [path, tag]

extractTag :: String -> StoreTag -> StoreTag
extractTag path (StoreTag tag) = cause ("extract " ++ show path) $ resolveTag' $ tag ++ ":" ++ if last path == '/' then path else path ++ "/"

resolveTag :: String -> Maybe StoreTag
resolveTag name = do
    name' <- escapeTagName name
    Right tag <- return $ unsafePerformIO $ resolveTag' $ name' ++ "^{tree}"
    return tag

nameTag :: String -> StoreTag -> IO Bool
nameTag name (StoreTag tag) = case escapeTagName name of
    Just name' -> do
        (Just i, Nothing, Nothing, p) <- createProcess (proc "git" ["tag", "-a", "-m", "", name', tag]) { std_in = CreatePipe }
        hClose i
        code <- waitForProcess p
        return $ code == ExitSuccess
    Nothing -> return False

data StoreFile =
    File { fileExecutable :: Bool, fileData :: B.ByteString } |
    Symlink { targetPath :: FilePath } |
    Hardlink { targetPath :: FilePath }

importTag :: [(FilePath, StoreFile)] -> IO StoreTag
importTag entries = do
    let ref = "refs/import-tmp"
    (Just stdin, Nothing, Nothing, p) <- createProcess (proc "git" ["fast-import", "--quiet", "--date-format=now"]) {
        std_in = CreatePipe,
        close_fds = True
    }
    hPutStr stdin $ unlines [
            "commit " ++ ref,
            "committer <apters@none> now",
            "data 0"
        ]
    forM_ entries $ \ (rawpath, storefile) ->
        let path = joinPath $ filter (\ d -> not (all (== '/') d || d == ".")) $ splitDirectories rawpath
        in case storefile of
        File exec contents -> do
            hPutStr stdin $ "M " ++ (if exec then "100755" else "100644") ++ " inline " ++ path ++ "\ndata " ++ show (B.length contents) ++ "\n"
            B.hPutStr stdin contents
            hPutStr stdin "\n"
        Symlink target -> hPutStr stdin $ "M 120000 inline " ++ path ++ "\ndata " ++ show (length target) ++ "\n" ++ target ++ "\n"
        Hardlink target -> hPutStr stdin $ "C " ++ target ++ " " ++ path ++ "\n"
    hClose stdin
    ExitSuccess <- waitForProcess p
    Right tag <- resolveTag' $ ref ++ "^{tree}"
    Right _ <- run "git" ["update-ref", "-d", ref]
    return tag

-- Internal helpers:

escapeTagName :: String -> Maybe String
escapeTagName name = do
    guard $ length name <= 64
    let isCharOK c = isAlphaNum c || c `elem` ".+-:~_"
    guard $ all isCharOK name
    return $ "store-" ++ escapeURIString (`notElem` ".:~") name

resolveTag' :: String -> IO (Either String StoreTag)
resolveTag' tag = storeTag "git" ["rev-parse", "--verify", tag]

storeTag :: String -> [String] -> IO (Either String StoreTag)
storeTag cmd args = do
    status <- run cmd args
    case status of
        Left err -> return $ Left err
        Right s -> case lines s of
            [l] | length l == 40 && all isHexDigit l -> return $ Right $ StoreTag l
            _ -> return $ Left "invalid tag returned from command"

cause :: String -> IO (Either String a) -> a
cause what act = case unsafePerformIO act of
    Right a -> a
    Left why -> error $ what ++ ": " ++ why

run :: String -> [String] -> IO (Either String String)
run cmd args = do
    (code, out, err) <- readProcessWithExitCode cmd args ""
    case (code, err) of
        (ExitSuccess, "") -> return $ Right out
        _ -> return $ Left err
