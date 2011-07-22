module Directory where

import Control.Exception
import Control.Monad
import System.Directory
import System.IO.Error
import System.Random

-- Note that this uses the current umask
mkdtemp :: FilePath -> IO FilePath
mkdtemp prefix = do
    suffix <- sequence $ replicate 8 $ randomRIO ('a', 'z')
    let path = prefix ++ '.' : suffix
    result <- tryJust (guard . isAlreadyExistsError) $ createDirectory path
    case result of
        Left _ -> mkdtemp prefix
        Right _ -> return path

-- FIXME: removeDirectoryRecursive follows symlinks!
withTemporaryDirectory :: FilePath -> (FilePath -> IO a) -> IO a
withTemporaryDirectory prefix act = bracket (mkdtemp prefix) (removeDirectoryRecursive) act
