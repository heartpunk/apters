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
    exportTag,
    StoreTag()
) where

import Directory

import Control.Concurrent
import Control.Monad
import qualified Data.ByteString as B
import Data.Char
import qualified Data.Enumerator as E
import qualified Data.Enumerator.Binary as EB
import Data.List
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
            (Just input, Nothing, Nothing, p) <- createProcess (proc "git" ("read-tree" : args)) {
                std_in = CreatePipe,
                env = Just environ,
                close_fds = True
            }
            hClose input
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
    let base arg0 args = CreateProcess {
            cmdspec = RawCommand arg0 args,
            cwd = Nothing,
            env = Just environ,
            std_in = CreatePipe,
            std_out = CreatePipe,
            std_err = CreatePipe,
            close_fds = True
        }
    let expectSilence arg0 args = do
        (Just i, Just o, Just e, p) <- createProcess $ base arg0 args
        hClose i
        o' <- hGetContents o
        e' <- hGetContents e
        allOutput <- mergeIO o' e'
        ex <- waitForProcess p
        case (allOutput, ex) of
            ("", ExitSuccess) -> return ()
            _ -> fail $ "running " ++ intercalate " " (arg0 : args) ++ ": " ++ show ex ++ ": " ++ allOutput

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
    (Just input, Nothing, Nothing, p) <- createProcess (proc "git" ["--git-dir", store, "tag", "-a", "-m", "", "cache-" ++ ident, tag]) { std_in = CreatePipe }
    hClose input
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

exportTag :: String -> StoreTag -> E.Iteratee B.ByteString IO a -> IO a
exportTag store (StoreTag tag) sink = do
    (Just input, Just output, Nothing, ph) <- createProcess (proc "git" ["--git-dir", store, "archive", "--format=tar", tag]) {
        std_in = CreatePipe,
        std_out = CreatePipe,
        close_fds = True
    }
    hClose input
    hSetBinaryMode output True
    result <- E.run_ $ EB.enumHandle 4096 output E.$$ sink
    ExitSuccess <- waitForProcess ph
    return result

-- Internal helpers:

resolveTag' :: String -> String -> IO (Either String StoreTag)
resolveTag' store tag = storeTag "git" ["--git-dir", store, "rev-parse", "--verify", tag] ""

storeTag :: String -> [String] -> String -> IO (Either String StoreTag)
storeTag cmd args input = do
    status <- run cmd args input
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
run cmd args input = do
    (code, out, err) <- readProcessWithExitCode cmd args input
    case (code, err) of
        (ExitSuccess, "") -> return $ Right out
        _ -> return $ Left err
