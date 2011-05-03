module Main where

import Control.Exception
import Control.Monad
import Data.List
import System.Directory
import System.FilePath
import System.Environment
import System.Exit
import System.IO.Error
import System.Process

import DepsScanner
import Store

clone :: [String] -> IO ()
clone [tag] = clone [tag, tag]
clone [tag, name] = do
    store <- readFile (".apters" </> "store")
    ExitSuccess <- rawSystem "git" ["init", name]
    let Just tag' = escapeTagName tag
    (Nothing, Nothing, Nothing, h) <- createProcess (proc "git" ["pull", store, "tag", tag']) { cwd = Just name }
    ExitSuccess <- waitForProcess h
    return ()
clone _ = putStrLn "Usage: apters clone <store tag> [<directory>]"

expand :: [String] -> IO ()
expand [parent, depname, child] = do
    depsStr <- readFile $ parent </> "apters.deps"
    let Just dep = lookup depname (getDeps depsStr)
    clone [dep, child]
    links <- readLinks
    writeLinks $ (parent, depname, child) : [l | l@(parent', depname', _) <- links, parent /= parent', depname /= depname']
    return ()
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
cmds = [("clone", clone),
        ("expand", expand),
        ("workspace", workspace),
        ("help", help)]

readLinks :: IO [(String, String, String)]
readLinks = do
    handleJust (guard . isDoesNotExistError) (const $ return []) $ liftM (take3s . split '\0') $ readFile $ ".apters" </> "links"
    where
    take3s :: [a] -> [(a, a, a)]
    take3s [] = []
    take3s (x:y:z:xs) = (x, y, z) : take3s xs
    take3s _ = error "links file corrupt"
    split :: Eq a => a -> [a] -> [[a]]
    split _ [] = []
    split x xs = case break (== x) xs of (v, xs') -> v : split x (drop 1 xs')

writeLinks :: [(String, String, String)] -> IO ()
writeLinks links = do
    writeFile (".apters" </> "links.tmp") $ intercalate "\0" $ join3s links
    renameFile (".apters" </> "links.tmp") (".apters" </> "links")
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