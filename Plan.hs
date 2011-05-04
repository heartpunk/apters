module Plan (evalTag) where

import DepsScanner
import Language
import Store

import Data.List
import System.Environment
import System.FilePath.Posix

-- The planning phase yields a Tree.

data Tree = Fetch StoreTag | Prefix String Tree | Extract String Tree | Merge [Tree] | Build Tree String
    deriving Show

type Env = [(String, Value)]

data Value =
    LambdaV (Value -> Value) |
    DictionaryV Env |
    ListV [Value] |
    StringV String |
    TreeV Tree | -- used only by builtins
    DepV StoreTag -- used only by builtins

instance Show Value where
    show (LambdaV {}) = "(lambda)"
    show (DictionaryV env) = "{" ++ intercalate "," [ " " ++ name ++ " = " ++ show value | (name, value) <- env ] ++ " }"
    show (ListV values) = "[" ++ intercalate "," [ " " ++ show value | value <- values ] ++ " ]"
    show (StringV string) = show string
    show (TreeV tree) = show tree
    show (DepV tag) = show tag

valueType :: Value -> String
valueType (LambdaV {}) = "lambda"
valueType (DictionaryV {}) = "dictionary"
valueType (ListV {}) = "list"
valueType (StringV {}) = "string"
valueType (TreeV {}) = "tree"
valueType (DepV {}) = "dependency"

badValue :: String -> String -> Value -> a
badValue op expected actual = error $ "can't " ++ op ++ " on a " ++ valueType actual ++ "; expected a " ++ expected

eval :: Env -> Expr -> Value
eval env (Lambda name body) = LambdaV $ \ arg -> eval ((name, arg) : env) body
eval env (Apply lam arg) = case eval env lam of
    LambdaV f -> f $ eval env arg
    v -> badValue "apply" "lambda" v
eval env (Let binds body) = let env' = [ (name, eval env' expr) | (name, expr) <- binds ] ++ env in eval env' body
eval env (Ident name) = case lookup name env of
    Just value -> value
    Nothing -> error $ "no variable in scope named " ++ show name
eval env (Dictionary binds) = let env' = [ (name, eval (env' ++ env) expr) | (name, expr) <- binds ] in DictionaryV env'
eval env (Member on field) = case eval env on of
    DictionaryV dict -> case lookup field dict of
        Just value -> value
        Nothing -> error $ "no field in dictionary named " ++ show field
    v -> badValue ("get member " ++ show field) "dictionary" v
eval env (List exprs) = ListV $ map (eval env) exprs
eval _ (String s) = StringV s

builtins :: StoreTag -> [(String, StoreTag)] -> String -> Env
builtins basetag deps basepath = [
        ("call", LambdaV $ onString "call" $ doCall basetag deps basepath),
        ("import", LambdaV doImport),
        ("deps", DictionaryV [ (key, DepV dep) | (key, dep) <- deps ]),
        ("self", DepV basetag),
        ("build", binop doBuild),
        ("prefix", binop $ onString "prefix" doPrefix),
        ("extract", binop $ onString "extract" doExtract),
        ("merge", LambdaV doMerge)
    ]
    where
    binop = (LambdaV .) (LambdaV .)

    onString _ f (StringV s) = f s
    onString caller _ v = badValue caller "string" v

    fetchTag _ (TreeV tree) = tree
    fetchTag _ (DepV tag) = Fetch tag
    fetchTag caller v = badValue ("fetch for " ++ caller) "tree or dependency" v

    doPrefix path tree = TreeV $ Prefix path $ fetchTag "prefix" tree

    doExtract path tree = TreeV $ Extract path $ fetchTag "extract" tree

    doMerge (ListV []) = error "can't merge an empty list of trees"
    doMerge (ListV trees) = TreeV $ Merge $ map (fetchTag "merge") trees
    doMerge v = badValue "merge" "list" v

    doBuild tree (StringV cmd) = TreeV $ Build (fetchTag "build" tree) cmd
    doBuild _ v = badValue "build command" "string" v

doEval tag deps sourcepath = eval (builtins tag deps sourcepath) $ parseExpr $ readTagFile tag $ tail sourcepath

doCall tag deps basepath spec = doEval tag deps $ takeDirectory basepath </> spec

doImport (DepV tag) = case mapM getDep $ getDeps $ readTagFile tag "apters.deps" of
    Just deps -> doEval tag deps "/default.apters"
    Nothing -> error $ "tag not found in apters.deps of " ++ show tag
    where getDep (name, tagstr) = do tag <- resolveTag tagstr; return (name, tag)
doImport v = badValue "import" "dependency" v

evalTree :: Tree -> StoreTag
evalTree (Fetch tag) = tag
evalTree (Prefix path tree) = prefixTag path $ evalTree tree
evalTree (Extract path tree) = extractTag path $ evalTree tree
evalTree (Merge trees) = mergeTags $ map evalTree trees
evalTree (Build tree cmd) = buildTag (evalTree tree) cmd

evalTag :: String -> IO ()
evalTag name = do
    Just tag <- return $ resolveTag name
    let plan = doImport (DepV tag)
    print plan
    case plan of
        TreeV tree -> print $ evalTree tree
        _ -> return ()
