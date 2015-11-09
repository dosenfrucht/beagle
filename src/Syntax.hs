module Syntax where

import Data.Word

type Name = String

data Toplevel = TopFunDef Name [(Name, Type)] Type Expr
              | TopVarDef Name Type Expr
              deriving (Show, Eq)


data Expr = If Expr Expr Expr
          | Op String Expr Expr
          | Lit Word64
          | Str String
          | BoolConst Bool
          | Var Name
          | Funcall Name [Expr]
          | Let Name Expr Expr
          | Block [Expr]
          deriving (Show, Eq)

data Type = TyName Name
          | TyFun Type Type
          deriving (Show, Eq)
