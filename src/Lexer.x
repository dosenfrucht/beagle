{

module Lexer where


import Data.Word
}

%wrapper "basic"

$digit = [0-9]
$alpha = [a-zA-Z]

tokens :-
    $white+        ;
    "//".*         ;

    if    { \s -> TIf }
    else  { \s -> TElse }
    let   { \s -> TLet }
    in    { \s -> TIn }
    true  { \s -> TTrue }
    false { \s -> TFalse }


    $digit+                             { \s -> TNum (read s) }
    \".*\"                              { \s -> TString (tail (init s)) }
    [\_ $alpha] [$alpha $digit \_ \']*	{ \s -> TIden s }
    \-\> { \s -> TArrow }
    \+   { \s -> TOpPlus  }
    \-   { \s -> TOpMinus }
    \*   { \s -> TOpTimes }
    \/   { \s -> TOpDiv   }
    \%   { \s -> TOpMod   }
    \<   { \s -> TOpL }
    \>   { \s -> TOpG }
    \(   { \s -> TOP }
    \)   { \s -> TCP }
    \=\= { \s -> TOpEqual }
    \/\= { \s -> TOpNequal }
    \=   { \s -> TEqual }
    \:   { \s -> TColon }
    \;   { \s -> TSemicolon }
    \,   { \s -> TComma }
    \.   { \s -> TDot   }
    \>\> { \s -> TOpShr }
    \<\< { \s -> TOpShl }
    \&   { \s -> TOpAnd }
    \|   { \s -> TOpOr  }
    \^   { \s -> TOpXor }
    \{   { \s -> TOCBracket }
    \}   { \s -> TCCBracket }

{
lexer :: String -> [Token]
lexer = alexScanTokens
data Token = TNum Word64
           | TString String
           | TIden String
           | TArrow
           | TIf
           | TElse
           | TLet
           | TIn
           | TTrue
           | TFalse
           | TOpPlus
           | TOpMinus
           | TOpTimes
           | TOpDiv
           | TOpMod
           | TOpL
           | TOpG
           | TOpEqual
           | TOpNequal
           | TOP
           | TCP
           | TOpShr
           | TOpShl
           | TOpAnd
           | TOpOr
           | TOpXor
           | TEqual
           | TColon
           | TSemicolon
           | TComma
           | TDot
           | TOCBracket
           | TCCBracket
           deriving (Show)
}
