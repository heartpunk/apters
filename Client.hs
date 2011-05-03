module Main where

import System.Directory
import System.FilePath
import System.Environment
import System.Exit
import System.Process

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
        ("workspace", workspace),
        ("help", help)]

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> help []
        cmd:cmdargs -> case lookup cmd cmds of
            Just f -> f cmdargs
            Nothing -> help args
