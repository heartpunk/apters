{
module Language (Expr(..), parseExpr) where
import Scanner
}

%name parse
%tokentype { Token }
%token
    let { TokenLet }
    in { TokenIn }
    "=>" { TokenArrow }
    "=" { TokenEq }
    "{" { TokenOpenBrace }
    "}" { TokenCloseBrace }
    "[" { TokenOpenBracket }
    "]" { TokenCloseBracket }
    "(" { TokenOpenParen }
    ")" { TokenCloseParen }
    "," { TokenSep }
    id { TokenIdent $$ }

%%

Exp : Exp0 { $1 }
    | let sepBy1(Bind, ",") in Exp { Let $2 $4 }

Exp0
    : Exp1 { $1 }
    | Exp0 Exp1 { Apply $1 $2 }

Exp1
    : "{" sepEndBy(Bind, ",") "}" { Dictionary $2 }
    | "[" sepEndBy(Exp, ",") "]" { List $2 }
    | "(" Exp ")" { $2 }
    | Exp1 "=>" id { Member $1 $3 }
    | id { Ident $1 }

Bind : id Lambda { ($1, $2) }

Lambda
    : id Lambda { Lambda $1 $2 }
    | "=" Exp { $2 }

list(p) : rev_list(p) { reverse $1 }
rev_list(p)
    : rev_list(p) p { $2 : $1 }
    | {- empty -} { [] }

fst(p, q) : p q { $1 }
snd(p, q) : p q { $2 }

sepBy1(p, sep) : p list(snd(sep, p)) { $1 : $2 }

sepEndBy(p, sep)
    : rev_list(fst(p, sep)) p { reverse ($2 : $1) }
    | list(fst(p, sep)) { $1 }

{
data Expr =
    Ident String |
    Lambda String Expr |
    Apply Expr Expr |
    Let [(String, Expr)] Expr |
    Dictionary [(String, Expr)] |
    Member Expr String |
    List [Expr]
    deriving Show

happyError _ = error "parse error"

parseExpr = parse . getTokens
}
