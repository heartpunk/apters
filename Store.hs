module Store (
    readTagFile,
    mergeTags,
    buildTag,
    prefixTag,
    extractTag,
    CacheKey(..),
    getCachedBuild,
    putCachedBuild,
    escapeTagName,
    resolveTag,
    nameTag,
    StoreFile(..), importTag,
    exportTag,
    StoreTag()
) where

import Directory

import Control.Concurrent
import Control.Exception
import Control.Monad
import qualified Data.ByteString as B
import Data.Char
import Data.Int
import Data.Iteratee (Iteratee, Stream(..), joinIM, liftI, idoneM, throwErr, mapChunksM_)
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

readTagFile :: String -> StoreTag -> String -> String
readTagFile store (StoreTag tag) path = unsafePerformIO $
    cause ("can't read path " ++ show path ++ " from store tag " ++ tag) $
    run "git" ["--git-dir", store, "cat-file", "blob", tag ++ ":" ++ path] ""

mergeTags :: String -> [StoreTag] -> IO StoreTag
mergeTags store tags = withTemporaryDirectory "/tmp/apters-index" $ \ tmpdir -> do
    environ <- liftM ([("GIT_DIR", store), ("GIT_INDEX_FILE", tmpdir </> "index")] ++) getEnvironment
    Right (StoreTag empty) <- storeTag "git" ["--git-dir", store, "mktree"] ""
    -- Due to a git bug introduced in the 1.6.2.5 release, and fixed in commit
    -- b1f47514f207b0601de7b0936cf13b3c0ae70081, this function requires git
    -- version 1.7.2 or later.
    let readTree args = do
            (Just null, Nothing, Nothing, p) <- createProcess (proc "git" ("read-tree" : args)) {
                std_in = CreatePipe,
                env = Just environ,
                close_fds = True
            }
            hClose null
            ExitSuccess <- waitForProcess p
            return ()
    readTree [empty]
    forM_ tags $ \ (StoreTag tag) -> readTree ["-i", "-m", empty, tag]
    indexTag' environ

buildTag :: String -> StoreTag -> String -> IO StoreTag
buildTag store (StoreTag root) cmd = withTemporaryDirectory "/tmp/apters-build" $ \ tmpdir -> do
    let indexfile = tmpdir </> "index"
    let worktree = tmpdir </> "build"
    environ <- liftM ([("GIT_DIR", store), ("GIT_INDEX_FILE", indexfile), ("GIT_WORK_TREE", worktree)] ++) getEnvironment
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
    indexTag' environ

indexTag' :: [(String, String)] -> IO StoreTag
indexTag' environ = do
    (Just i2, Just o2, Just e2, p2) <- createProcess (proc "git" ["write-tree"]) {
            env = Just environ,
            std_in = CreatePipe,
            std_out = CreatePipe,
            std_err = CreatePipe,
            close_fds = True
        }
    hClose i2
    o' <- hGetContents o2
    e' <- hGetContents e2
    ex <- waitForProcess p2
    case (lines o', e', ex) of
        ([l], "", ExitSuccess) | length l == 40 && all isHexDigit l -> return $ StoreTag l
        _ -> fail $ "running git write-tree: " ++ show ex ++ ": " ++ o' ++ e'

prefixTag :: String -> String -> StoreTag -> IO StoreTag
prefixTag store path tag = cause ("prefix " ++ show path) $ foldM prefixOne (Right tag) $ reverse dirs
    where
    prefixOne (Right (StoreTag tree)) dir = storeTag "git" ["--git-dir", store, "mktree"] $ "040000 tree " ++ tree ++ "\t" ++ dir ++ "\n"
    prefixOne e _ = return e
    dirs = case splitDirectories path of
        ('/' : _) : rest -> rest
        l -> l

extractTag :: String -> String -> StoreTag -> IO StoreTag
extractTag store path (StoreTag tag) = cause ("extract " ++ show path) $ resolveTag' store $ tag ++ ":" ++ if last path == '/' then path else path ++ "/"

class CacheKey a where
    cacheKeyIdent :: a -> String

instance CacheKey StoreTag where
    cacheKeyIdent (StoreTag tag) = tag

instance (CacheKey a, CacheKey b) => CacheKey (a, b) where
    cacheKeyIdent (a, b) = cacheKeyIdent a ++ "-" ++ cacheKeyIdent b

getCachedBuild :: CacheKey k => String -> k -> IO (Maybe StoreTag)
getCachedBuild store key = liftM (either (const Nothing) Just) $ resolveTag' store $ "cache-" ++ cacheKeyIdent key ++ "^{tree}"

putCachedBuild :: CacheKey k => String -> k -> StoreTag -> IO ()
putCachedBuild store key (StoreTag tag) = do
    let ident = cacheKeyIdent key
    (Just null, Nothing, Nothing, p) <- createProcess (proc "git" ["--git-dir", store, "tag", "-a", "-m", "", "cache-" ++ ident, tag]) { std_in = CreatePipe }
    hClose null
    code <- waitForProcess p
    when (code /= ExitSuccess) $ putStrLn $ "apters: warning: failed to cache build " ++ ident

escapeTagName :: String -> Maybe String
escapeTagName name = do
    hash <- stripPrefix "git/" name
    guard $ length hash == 40 && all isHexDigit hash
    return hash

resolveTag :: String -> String -> Maybe StoreTag
resolveTag store name = do
    name' <- escapeTagName name
    Right tag <- return $ unsafePerformIO $ resolveTag' store $ name' ++ "^{tree}"
    return tag

nameTag :: String -> String -> StoreTag -> IO Bool
nameTag store name (StoreTag tag) = case escapeTagName name of
    Just name' -> do
        environ <- getEnvironment
        (Just null1, Just hashpipe, Nothing, p1) <- createProcess (proc "git" ["--git-dir", store, "commit-tree", tag]) { std_in = CreatePipe, std_out = CreatePipe, env = Just $ [
            ("GIT_AUTHOR_NAME", "Apters"),
            ("GIT_AUTHOR_EMAIL", "apters@example"),
            ("GIT_AUTHOR_DATE", "1970-01-01 00:00:00 +0000"),
            ("GIT_COMMITTER_NAME", "Apters"),
            ("GIT_COMMITTER_EMAIL", "apters@example"),
            ("GIT_COMMITTER_DATE", "1970-01-01 00:00:00 +0000")] ++ environ }
        hClose null1
        commit <- hGetLine hashpipe
        hClose hashpipe
        code1 <- waitForProcess p1
        if code1 /= ExitSuccess then return False else do
        (Just null2, Nothing, Nothing, p2) <- createProcess (proc "git" ["--git-dir", store, "tag", "-a", "-m", "", name', commit]) { std_in = CreatePipe }
        hClose null2
        code2 <- waitForProcess p2
        return $ code2 == ExitSuccess
    Nothing -> return False

data StoreFile =
    File { fileExecutable :: Bool, fileSize :: Int64, fileData :: Iteratee B.ByteString IO () -> IO () } |
    Symlink { targetPath :: FilePath } |
    Hardlink { targetPath :: FilePath }

importTag :: String -> Iteratee (FilePath, StoreFile) IO StoreTag
importTag store = joinIM $ do
    let ref = "refs/import-tmp"
    (Just stdin, Nothing, Nothing, p) <- createProcess (proc "git" ["--git-dir", store, "fast-import", "--quiet", "--date-format=raw"]) {
        std_in = CreatePipe,
        close_fds = True
    }
    hPutStr stdin $ unlines [
            "commit " ++ ref,
            "committer Apters <apters@example> 0 +0000",
            "data 0"
        ]
    let cleanup maybeExc = do
            hClose stdin
            result <- waitForProcess p
            tryTag <- resolveTag' store $ ref ++ "^{tree}"
            Right _ <- run "git" ["--git-dir", store, "update-ref", "-d", ref] ""
            case (maybeExc, result, tryTag) of
                (Nothing, ExitSuccess, Right tag) -> return tag
                (Just e, _, _) -> throw e
                (_, ExitFailure _, _) -> fail $ "git fast-import: " ++ show result
                (_, _, Left notag) -> fail $ "git fast-import: " ++ notag
    let cleanPath rawpath = joinPath $ filter (\ d -> not (all (== '/') d || d == ".")) $ splitDirectories rawpath
    return $ forI cleanup $ \ (rawpath, storefile) ->
        let path = cleanPath rawpath
        in case storefile of
        File exec size contents -> do
            hPutStr stdin $ "M " ++ (if exec then "100755" else "100644") ++ " inline " ++ path ++ "\ndata " ++ show size ++ "\n"
            contents $ mapChunksM_ $ B.hPutStr stdin
            hPutStr stdin "\n"
        Symlink target -> hPutStr stdin $ "M 120000 inline " ++ path ++ "\ndata " ++ show (length target) ++ "\n" ++ target ++ "\n"
        Hardlink target -> hPutStr stdin $ "C " ++ cleanPath target ++ " " ++ path ++ "\n"

exportTag :: String -> StoreTag -> FilePath -> IO ()
exportTag store (StoreTag tag) file = do
    ExitSuccess <- rawSystem "git" ["--git-dir", store, "archive", "-o", file, tag]
    return ()

-- Internal helpers:

forI :: (Maybe SomeException -> IO a) -> (b -> IO ()) -> Iteratee b IO a
forI end act = go
    where
    go = liftI $ \ s -> case s of
        EOF e -> stop e
        Chunk b -> joinIM $ do
            result <- try $ act b
            case result of
                Left e -> return $ stop $ Just e
                Right () -> return go
    stop maybeExc = joinIM $ do
        result <- try $ end maybeExc
        case result of
            Left e -> return $ throwErr e
            Right a -> idoneM a $ EOF Nothing

resolveTag' :: String -> String -> IO (Either String StoreTag)
resolveTag' store tag = storeTag "git" ["--git-dir", store, "rev-parse", "--verify", tag] ""

storeTag :: String -> [String] -> String -> IO (Either String StoreTag)
storeTag cmd args stdin = do
    status <- run cmd args stdin
    case status of
        Left err -> return $ Left err
        Right s -> case lines s of
            [l] | length l == 40 && all isHexDigit l -> return $ Right $ StoreTag l
            _ -> return $ Left "invalid tag returned from command"

cause :: String -> IO (Either String a) -> IO a
cause what act = do
    result <- act
    case result of
        Right a -> return a
        Left why -> fail $ what ++ ": " ++ why

run :: String -> [String] -> String -> IO (Either String String)
run cmd args stdin = do
    (code, out, err) <- readProcessWithExitCode cmd args stdin
    case (code, err) of
        (ExitSuccess, "") -> return $ Right out
        _ -> return $ Left err
