{
module Parser where

import Lexer
import Syntax

}

%name parse Toplevel
%name parseType type
%name parseVar vardef
%name parseName qualname
%name parseTest test
%tokentype { Token }
%monad { Either String }
%error { parseError }

%token
    if   { TIf }
    else { TElse }
    let  { TLet }
    in   { TIn }
    true  { TTrue }
    false { TFalse }
    iden { TIden $$ }
    num  { TNum $$ }
    str  { TString $$ }
    '->' { TArrow }
    '('  { TOP }
    ')'  { TCP }
    '+'  { TOpPlus }
    '-'  { TOpMinus }
    '*'  { TOpTimes }
    '/'  { TOpDiv }
    '%'  { TOpMod }
    '<'  { TOpL }
    '>'  { TOpG }
    '==' { TOpEqual }
    '/=' { TOpNequal }
    '>>' { TOpShr }
    '<<' { TOpShl }
    '&'  { TOpAnd }
    '|'  { TOpOr }
    '^'  { TOpXor }
    '='  { TEqual }
    ':'  { TColon }
    ';'  { TSemicolon }
    ','  { TComma }
    '.'  { TDot }
    '{'  { TOCBracket }
    '}'  { TCCBracket }
    '\\' { TLam }


%nonassoc '==' '/='
%nonassoc '>' '<'
%left '>>' '<<'
%left '+' '-'
%left '*' '/' '%'
%right '&' '|' '^'


%%



test : qualname ':' { ($1, ':') }



Toplevel : toplevels { $1 }

toplevels : toplevels toplevel { $2 : $1 }
          | toplevel           { [$1]    }
          | {- empty -}        { [] }

toplevel : functiondef { $1 }
         | vardef      { $1 }

qualname : iden        { $1    }

type : type1 '->' type { TyFun $1 $3 }
     | type1           { $1 }

type1 : qualname       { TyName $1 }
      | '(' type ')'   { $2 }

functiondef : qualname '(' params ')' ':' type '=' expr ';'   { TopFunDef $1 (reverse $3) $6 $8 }

params : params ',' qualname ':' type { ($3, $5) : $1    }
       | qualname ':' type            { [($1, $3)] }

vardef : qualname ':' type '=' expr ';' { TopVarDef $1 $3 $5 }

expr : if '(' expr ')' expr else expr  { If $3 $5 $7 }
     | let qualname '=' expr in expr       { Let $2 $4 $6 }
     | '{' block '}'                   { Block (reverse $2) }
     | expr '==' expr { Op "==" $1 $3 }
     | expr '/=' expr { Op "/=" $1 $3 }
     | expr '<<' expr { Op "<<" $1 $3 }
     | expr '>>' expr { Op ">>" $1 $3 }
     | expr '&' expr  { Op "&" $1 $3  }
     | expr '|' expr  { Op "|" $1 $3  }
     | expr '^' expr  { Op "^" $1 $3  }
     | expr '<' expr  { Op "<" $1 $3  }
     | expr '>' expr  { Op ">" $1 $3  }
     | expr '+' expr  { Op "+" $1 $3  }
     | expr '-' expr  { Op "-" $1 $3  }
     | expr '*' expr  { Op "*" $1 $3  }
     | expr '/' expr  { Op "/" $1 $3  }
     | expr '%' expr  { Op "%" $1 $3  }
     | factor         { $1 }

block : expr ';'        { [$1] }
      | block expr ';'  { $2 : $1 }

factor : expr '(' args ')' { Funcall $1 (reverse $3) }
       | num               { Lit $1 }
       | str               { Str $1 }
       | '\\' '(' qualname ':' type ')' '->' expr { Lam $3 $5 $8 }
       | true  { BoolConst True }
       | false { BoolConst False }
       | qualname              { Var $1 }
       | '(' expr ')'      { $2 }

args : args ',' expr { $3 : $1 }
     | expr          { [$1]    }

{

parseError t = Left $ "Parser error: " ++ (show t)

}
