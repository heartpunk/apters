import Language

import Data.List

-- The planning phase yields a Tree.

data Tree = Fetch String | Prefix String Tree | Extract String Tree | Merge [Tree] | Build Tree String
    deriving Show

type Env = [(String, Value)]

data Value =
    LambdaV (Value -> Value) |
    DictionaryV Env |
    ListV [Value] |
    StringV String |
    TreeV Tree -- used only by builtins

instance Show Value where
    show (LambdaV {}) = "(lambda)"
    show (DictionaryV env) = "{" ++ intercalate "," [ " " ++ name ++ " = " ++ show value | (name, value) <- env ] ++ " }"
    show (ListV values) = "[" ++ intercalate "," [ " " ++ show value | value <- values ] ++ " ]"
    show (StringV string) = show string
    show (TreeV tree) = show tree

valueType :: Value -> String
valueType (LambdaV {}) = "lambda"
valueType (DictionaryV {}) = "dictionary"
valueType (ListV {}) = "list"
valueType (StringV {}) = "string"
valueType (TreeV {}) = "tree"

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
    Nothing -> StringV name
eval env (Dictionary binds) = let env' = [ (name, eval (env' ++ env) expr) | (name, expr) <- binds ] in DictionaryV env'
eval env (Member on field) = case eval env on of
    DictionaryV dict -> case lookup field dict of
        Just value -> value
        Nothing -> error $ "no field in dictionary named " ++ show field
    v -> badValue ("get member " ++ show field) "dictionary" v
eval env (List exprs) = ListV $ map (eval env) exprs

builtins :: Env
builtins = [
        ("import", binop doImport),
        ("build", binop doBuild),
        ("prefix", binop doPrefix),
        ("extract", binop doExtract),
        ("merge", LambdaV doMerge)
    ]
    where
    binop = (LambdaV .) (LambdaV .)

    doImport tag path = error "import not implemented yet"

    fetchTag _ (TreeV tree) = tree
    fetchTag _ (StringV tag) = Fetch tag
    fetchTag caller v = badValue ("fetch for " ++ caller) "tree or string" v

    doPrefix (StringV path) tree = TreeV $ Prefix path $ fetchTag "prefix" tree
    doPrefix v _ = badValue "prefix" "string" v

    doExtract (StringV path) tree = TreeV $ Extract path $ fetchTag "extract" tree
    doExtract v _ = badValue "extract" "string" v

    doMerge (ListV []) = error "can't merge an empty list of trees"
    doMerge (ListV trees) = TreeV $ Merge $ map (fetchTag "merge") trees
    doMerge v = badValue "merge" "list" v

    doBuild tree (StringV cmd) = TreeV $ Build (fetchTag "build" tree) cmd
    doBuild _ v = badValue "build command" "string" v

main = do
    program <- getContents
    print $ eval builtins $ parseExpr program
