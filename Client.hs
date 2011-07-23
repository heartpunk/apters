module Main where

import Control.Monad
import qualified Data.Enumerator.Binary as EB
import Data.List
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.IO
import System.Process

import DepsScanner
import Store.Base
import Store.File
import Store.Util

getStore :: String -> IO Store
getStore path = liftM fileStore $ readFile (path </> ".apters" </> "store")

cmdBuild :: [String] -> IO ()
cmdBuild [file] = do
    store <- getStore ".."
    (ExitSuccess, out, "") <- readProcessWithExitCode "git" ["rev-parse", "--verify", "HEAD^{tree}"] ""
    let [tag] = lines out
    maybeResult <- build store ("git/" ++ tag)
    case maybeResult of
        Just result -> do
            print result
            withFile file WriteMode $ export store result . EB.iterHandle
        Nothing -> putStrLn "Recipe didn't evaluate to a tree; can't export it."
cmdBuild _ = putStrLn "Usage: apters build <output file>"

cmdClone :: [String] -> IO ()
cmdClone [tag] = cmdClone [tag, tag]
cmdClone [tag, name] = do
    store <- getStore "."
    case parseGitReference tag of
        Just hash -> do
            repos <- findRepos store tag
            ExitSuccess <- rawSystem "git" ["init", name]
            forM_ repos $ \(reponame, repo) -> do
                ExitSuccess <- rawSystem "git" ["--git-dir", name </> ".git", "remote", "add", "-f", reponame, repo]
                return ()
            (ExitSuccess, out, "") <- readProcessWithExitCode "git" ["--git-dir", name </> ".git", "for-each-ref", "refs/remotes/"] ""
            let refs = [ref | [hash', "commit", refname] <- map words $ lines out, hash == hash', not $ "/HEAD" `isSuffixOf` refname, let Just ref = stripPrefix "refs/remotes/" refname]
            let args = case refs of
                    [ref] -> ["--track", ref]
                    _ -> [hash]
            (Nothing, Nothing, Nothing, h) <- createProcess (proc "git" (["checkout"] ++ args ++ ["--"])) { cwd = Just name }
            ExitSuccess <- waitForProcess h
            when (length refs > 1) $ do
                putStrLn $ "\nThe commit you've requested matches the head of several branches:"
                forM_ refs $ \ref -> putStrLn $ "  " ++ ref
                putStrLn "You might want to \"git checkout --track $branch\" on one of those branches."
            return ()
        Nothing -> do
            maybeRepo <- getRepo store tag
            case maybeRepo of
                Just repo -> do
                    ExitSuccess <- rawSystem "git" ["clone", "--origin", tag, repo, name]
                    return ()
                Nothing -> putStrLn $ "The store does not contain a repo named \"" ++ tag ++ "\"."
cmdClone _ = putStrLn "Usage: apters clone <store tag> [<directory>]"

cmdCommit :: [String] -> IO ()
cmdCommit [] = do
    (ExitSuccess, out, "") <- readProcessWithExitCode "git" ["rev-parse", "--verify", "HEAD^{commit}"] ""
    let [tag] = lines out
    child <- liftM takeFileName getCurrentDirectory
    links <- liftM readLinks $ readFileOrEmpty $ ".." </> ".apters" </> "links"
    forM_ [(parent, dep) | (parent, dep, child') <- links, child == child'] $ \ (parent, dep) -> do
        interactFile (".." </> parent </> "apters.deps") $ \ deps ->
            showDeps $ (dep, "git/" ++ tag) : [p | p@(dep', _) <- getDeps deps, dep /= dep']
cmdCommit _ = putStrLn "Usage: apters commit"

cmdExpand :: [String] -> IO ()
cmdExpand [parent, depname, child] = do
    depsStr <- readFile $ parent </> "apters.deps"
    let Just dep = lookup depname (getDeps depsStr)
    cmdClone [dep, child]
    interactFile (".apters" </> "links") $ \ links ->
        showLinks $ (parent, depname, child) : [l | l@(parent', depname', _) <- readLinks links, not $ parent == parent' && depname == depname']
cmdExpand _ = putStrLn "Usage: apters expand <parent_dir> <dependency> <child_dir>"

cmdNewrepo :: [String] -> IO ()
cmdNewrepo [name] = do
    store <- getStore "."
    success <- newRepo store name
    if success then cmdClone [name] else putStrLn "Could not create repository."
cmdNewrepo _ = putStrLn "Usage: apters newrepo <name>"

cmdNewstore :: [String] -> IO ()
cmdNewstore [store] = do
    createDirectoryIfMissing True (store </> "repos")
    ExitSuccess <- rawSystem "git" ["init", "--bare", store </> "cache.git"]
    return ()
cmdNewstore _ = putStrLn "Usage: apters newstore <name>"

cmdWorkspace :: [String] -> IO ()
cmdWorkspace [url, dir] = do
    createDirectory dir
    let aptersDir = dir </> ".apters"
    createDirectory aptersDir
    writeFile (aptersDir </> "store") url
cmdWorkspace _ = putStrLn "Usage: apters workspace <store url> <directory>"

cmdHelp :: [String] -> IO ()
cmdHelp [] = do
    putStrLn "Usage: apters <command> [<args>]\n\nApters commands:"
    mapM_ (putStrLn . fst) cmds
cmdHelp (cmd:_) = case lookup cmd cmds of
    Just _ -> putStrLn $ "apters: no help on " ++ cmd ++ " for you!"
    Nothing -> putStrLn $ "apters: '" ++ cmd ++ "' is not an apters command. See 'apters help'."

cmds :: [(String, [String] -> IO ())]
cmds = [("build", cmdBuild),
        ("clone", cmdClone),
        ("commit", cmdCommit),
        ("expand", cmdExpand),
        ("newrepo", cmdNewrepo),
        ("newstore", cmdNewstore),
        ("workspace", cmdWorkspace),
        ("help", cmdHelp)]

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
        [] -> cmdHelp []
        cmd:cmdargs -> case lookup cmd cmds of
            Just f -> f cmdargs
            Nothing -> cmdHelp args
