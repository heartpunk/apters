module Store.Base where

import Data.ByteString
import Data.Enumerator

data Store = Store {
    build :: String -> IO (Maybe String),
    export :: String -> Iteratee ByteString IO () -> IO (),
    findRepos :: String -> IO [(String, String)],
    getRepo :: String -> IO (Maybe String),
    newRepo :: String -> IO Bool
}
