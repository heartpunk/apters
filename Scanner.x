{
module Scanner (Token(..), getTokens) where
}

%wrapper "basic"

tokens :-
    $white+ ;
    "#" .* ;
    "let" { const TokenLet }
    "in" { const TokenIn }
    "=>" { const TokenArrow }
    "=" { const TokenEq }
    "{" { const TokenOpenBrace }
    "}" { const TokenCloseBrace }
    "[" { const TokenOpenBracket }
    "]" { const TokenCloseBracket }
    "(" { const TokenOpenParen }
    ")" { const TokenCloseParen }
    "," { const TokenSep }
    [0-9A-Za-z_':\-\~\+\.\/]+ { TokenIdent }

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
    TokenIdent String
    deriving Show

getTokens :: String -> [Token]
getTokens = alexScanTokens
}
