module Store.Base where

import Data.ByteString
import Data.Enumerator

import Store

data Store = Store {
    build :: String -> IO (Maybe StoreTag),
    export :: StoreTag -> Iteratee ByteString IO () -> IO (),
    findRepos :: String -> IO [(String, String)],
    getRepo :: String -> IO (Maybe String),
    newRepo :: String -> IO Bool
}
