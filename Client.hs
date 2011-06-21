module Main where

import Control.Exception
import Control.Monad
import Data.List
import System.Directory
import System.FilePath
import System.Environment
import System.Exit
import System.IO.Error
import System.Posix.Env
import System.Process

import DepsScanner
import Plan
import Store

build :: [String] -> IO ()
build [tag] = do
    store <- readFile (".apters" </> "store")
    evalTag store tag
build _ = putStrLn "Usage: apters build <store tag>"

clone :: [String] -> IO ()
clone [tag] = clone [tag, tag]
clone [tag, name] = do
    store <- readFile (".apters" </> "store")
    ExitSuccess <- rawSystem "git" ["clone", store, name]
    let Just tag' = escapeTagName tag
    (Nothing, Nothing, Nothing, h) <- createProcess (proc "git" ["-c", "advice.detachedHead=false", "checkout", tag', "--"]) { cwd = Just name }
    ExitSuccess <- waitForProcess h
    return ()
clone _ = putStrLn "Usage: apters clone <store tag> [<directory>]"

commit :: [String] -> IO ()
commit [] = do
    (ExitSuccess, out, "") <- readProcessWithExitCode "git" ["rev-parse", "--verify", "HEAD^{commit}"] ""
    let [tag] = lines out
    child <- liftM takeFileName getCurrentDirectory
    links <- liftM readLinks $ readFileOrEmpty $ ".." </> ".apters" </> "links"
    forM_ [(parent, dep) | (parent, dep, child') <- links, child == child'] $ \ (parent, dep) -> do
        interactFile (".." </> parent </> "apters.deps") $ \ deps ->
            showDeps $ (dep, "git/" ++ tag) : [p | p@(dep', _) <- getDeps deps, dep /= dep']
commit _ = putStrLn "Usage: apters commit"

expand :: [String] -> IO ()
expand [parent, depname, child] = do
    depsStr <- readFile $ parent </> "apters.deps"
    let Just dep = lookup depname (getDeps depsStr)
    clone [dep, child]
    interactFile (".apters" </> "links") $ \ links ->
        showLinks $ (parent, depname, child) : [l | l@(parent', depname', _) <- readLinks links, not $ parent == parent' && depname == depname']
expand _ = putStrLn "Usage: apters expand <parent_dir> <dependency> <child_dir>"

workspace :: [String] -> IO ()
workspace [url, dir] = do
    createDirectory dir
    let aptersDir = dir </> ".apters"
    createDirectory aptersDir
    writeFile (aptersDir </> "store") url
workspace _ = putStrLn "Usage: apters workspace <store url> <directory>"

help :: [String] -> IO ()
help [] = do
    putStrLn "Usage: apters <command> [<args>]\n\nApters commands:"
    mapM_ (putStrLn . fst) cmds
help (cmd:_) = case lookup cmd cmds of
    Just _ -> putStrLn $ "apters: no help on " ++ cmd ++ " for you!"
    Nothing -> putStrLn $ "apters: '" ++ cmd ++ "' is not an apters command. See 'apters help'."

cmds :: [(String, [String] -> IO ())]
cmds = [("build", build),
        ("clone", clone),
        ("commit", commit),
        ("expand", expand),
        ("workspace", workspace),
        ("help", help)]

readFileOrEmpty :: FilePath -> IO String
readFileOrEmpty = handleJust (guard . isDoesNotExistError) (const $ return "") . readFile

interactFile :: FilePath -> (String -> String) -> IO ()
interactFile file f = do
    let tmp = addExtension file "tmp"
    old <- readFileOrEmpty file
    writeFile tmp $ f old
    renameFile tmp file

showDeps :: [(String, String)] -> String
showDeps deps = unlines [key ++ "=" ++ value | (key, value) <- sort deps ]

readLinks :: String -> [(String, String, String)]
readLinks = take3s . split '\0'
    where
    take3s :: [a] -> [(a, a, a)]
    take3s [] = []
    take3s (x:y:z:xs) = (x, y, z) : take3s xs
    take3s _ = error "links file corrupt"
    split :: Eq a => a -> [a] -> [[a]]
    split _ [] = []
    split x xs = case break (== x) xs of (v, xs') -> v : split x (drop 1 xs')

showLinks :: [(String, String, String)] -> String
showLinks = intercalate "\0" . join3s
    where
    join3s :: [(a, a, a)] -> [a]
    join3s [] = []
    join3s ((x, y, z) : xs) = x:y:z:join3s xs

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> help []
        cmd:cmdargs -> case lookup cmd cmds of
            Just f -> f cmdargs
            Nothing -> help args
