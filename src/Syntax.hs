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
          | Lam Name Type Expr
          | Funcall Expr [Expr]
          | Let Name Expr Expr
          | Block [Expr]
          deriving (Show, Eq)

data Type = TyName Name
          | TyFun Type Type
          deriving (Eq)

instance Show Type where
    show (TyName n)  = n
    show (TyFun l r) = "(" ++ show l ++ " -> " ++ show r ++ ")"

data CoreToplevel = CTopVarDef Name Type CoreExpr
                  deriving (Show, Eq)

data CoreExpr = CIf CoreExpr CoreExpr CoreExpr
              | CLit Word64
              | CStr String
              | CBool Bool
              | CVar String
              | CLam Name Type CoreExpr
              | CApp CoreExpr CoreExpr
              | CLet Name CoreExpr CoreExpr
              | CSeq CoreExpr CoreExpr
              deriving (Show, Eq)
