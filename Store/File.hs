{-# LANGUAGE RecordWildCards #-}
module Store.File where

import Control.Monad
import System.Directory
import System.Exit
import System.FilePath
import System.Process

import Plan
import Store
import Store.Base
import Store.Util

fileStore :: FilePath -> Store
fileStore store = Store { .. }
    where
    cache = store </> "cache.git"
    reposDir = store </> "repos"

    validName name = '/' `notElem` name

    build tag = evalTag cache tag
    export result = exportTag cache result

    findRepos name = case parseGitReference name of
        Just hash -> do
            possibleRepos <- getDirectoryContents reposDir
            let containsCommit repo = fmap (== ExitSuccess) $ rawSystem "git" ["--git-dir", reposDir </> repo ++ ".git", "rev-parse", "--quiet", "--no-revs", "--verify", hash ++ "^{commit}"]
            repos <- filterM containsCommit [repo | (repo, ".git") <- map splitExtension possibleRepos]
            return [ (repo, reposDir </> repo) | repo <- repos ]
        Nothing -> return []

    getRepo name | validName name = do
        let path = reposDir </> name
        exists <- doesDirectoryExist $ path ++ ".git"
        return $ if exists then Just path else Nothing
    getRepo _ = return Nothing

    newRepo name | not (validName name) = return False
    newRepo name = do
        let path = "repos" </> name ++ ".git"
        ExitSuccess <- rawSystem "git" ["init", "--bare", store </> path]
        let alternates = store </> "cache.git" </> "objects" </> "info" </> "alternates"
        let objects = ".." </> ".." </> path </> "objects"
        interactFile alternates (unlines . (++ [objects]) . lines)
        return True
