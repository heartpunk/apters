module Store (
    readTagFile,
    mergeTags,
    buildTag,
    prefixTag,
    extractTag,
    resolveTag,
    nameTag,
    StoreTag()
) where

import Control.Monad
import Data.Char
import Network.URI (escapeURIString)
import System.Exit
import System.IO.Unsafe
import System.Process

newtype StoreTag = StoreTag String deriving Show

readTagFile :: StoreTag -> String -> String
readTagFile (StoreTag tag) path = cause ("can't read path " ++ show path ++ " from store tag " ++ tag) $
    unsafeRun "git" ["cat-file", "blob", tag ++ ":" ++ path]

mergeTags :: [StoreTag] -> StoreTag
mergeTags tags = cause "merge" $ storeTag "./merge-trees" [ tag | StoreTag tag <- tags ]

buildTag :: StoreTag -> String -> StoreTag
buildTag (StoreTag root) cmd = cause "build" $ storeTag "./build" [root, cmd]

prefixTag :: String -> StoreTag -> StoreTag
prefixTag path (StoreTag tag) = cause ("prefix " ++ show path) $ storeTag "./prefix" [path, tag]

extractTag :: String -> StoreTag -> StoreTag
extractTag path (StoreTag tag) = cause ("extract " ++ show path) $ resolveTag' $ tag ++ ":" ++ if last path == '/' then path else path ++ "/"

resolveTag :: String -> Maybe StoreTag
resolveTag name = do
    name' <- escapeTagName name
    Right tag <- return $ resolveTag' $ name' ++ "^{tree}"
    return tag

nameTag :: String -> StoreTag -> IO Bool
nameTag name (StoreTag tag) = case escapeTagName name of
    Just name' -> do
        (code, _out, _err) <- readProcessWithExitCode "git" ["tag", "-a", "-m", "", name', tag] ""
        return $ code == ExitSuccess
    Nothing -> return False

-- Internal helpers:

escapeTagName :: String -> Maybe String
escapeTagName name = do
    guard $ length name <= 64
    let isCharOK c = isAlphaNum c || c `elem` ".+-:~_"
    guard $ all isCharOK name
    return $ "store-" ++ escapeURIString (`notElem` ".:~") name

resolveTag' :: String -> Either String StoreTag
resolveTag' tag = storeTag "git" ["rev-parse", "--verify", tag]

storeTag :: String -> [String] -> Either String StoreTag
storeTag cmd args = case unsafeRun cmd args of
    Left err -> Left err
    Right s -> case lines s of
        [l] | length l == 40 && all isHexDigit l -> Right $ StoreTag l
        _ -> Left "invalid tag returned from command"

cause :: String -> Either String a -> a
cause _ (Right a) = a
cause what (Left why) = error $ what ++ ": " ++ why

unsafeRun :: String -> [String] -> Either String String
unsafeRun cmd args = unsafePerformIO $ do
    (code, out, err) <- readProcessWithExitCode cmd args ""
    case (code, err) of
        (ExitSuccess, "") -> return $ Right out
        _ -> return $ Left err
