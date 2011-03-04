{
module Scanner (Token(..), getTokens) where
}

%wrapper "basic"

tokens :-
    $white+ ;
    "#" .* ;
    "let" { const TokenLet }
    "in" { const TokenIn }
    "." { const TokenArrow }
    "=" { const TokenEq }
    "{" { const TokenOpenBrace }
    "}" { const TokenCloseBrace }
    "[" { const TokenOpenBracket }
    "]" { const TokenCloseBracket }
    "(" { const TokenOpenParen }
    ")" { const TokenCloseParen }
    "," { const TokenSep }
    \" [^\"]* \" { TokenString . init . tail }
    [a-zA-Z_] [0-9a-zA-Z_]* { TokenIdent }

{
data Token =
    TokenLet |
    TokenIn |
    TokenArrow |
    TokenEq |
    TokenOpenBrace |
    TokenCloseBrace |
    TokenOpenBracket |
    TokenCloseBracket |
    TokenOpenParen |
    TokenCloseParen |
    TokenSep |
    TokenString String |
    TokenIdent String
    deriving Show

getTokens :: String -> [Token]
getTokens = alexScanTokens
}
