module Store.Util where

import Control.Exception
import Control.Monad
import System.Directory
import System.FilePath
import System.IO.Error

readFileOrEmpty :: FilePath -> IO String
readFileOrEmpty = handleJust (guard . isDoesNotExistError) (const $ return "") . readFile

interactFile :: FilePath -> (String -> String) -> IO ()
interactFile file f = do
    let tmp = addExtension file "tmp"
    old <- readFileOrEmpty file
    writeFile tmp $ f old
    renameFile tmp file
