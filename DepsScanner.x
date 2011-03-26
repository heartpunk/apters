{
module DepsScanner (getDeps) where
import Control.Arrow (second)
}

%wrapper "basic"

tokens :-
    [a-zA-Z_] [0-9a-zA-Z_]* "=" .+ \n { second (init . tail) . break (== '=') }

{
getDeps :: String -> [(String, String)]
getDeps = alexScanTokens
}
