module Desugar (
        desugar
    )where

import Syntax

desugar :: [Toplevel] -> [CoreToplevel]
desugar = map desugarTop

desugarTop :: Toplevel -> CoreToplevel
desugarTop (TopVarDef n    t e) = CTopVarDef n t  (desugarExpr e )
desugarTop (TopFunDef n as t e) = CTopVarDef n t' e'
    where aNames = map fst as
          aTypes = map snd as

          t' = foldr TyFun t aTypes
          e' = foldr (\(n, t) acc -> CLam n t acc) (desugarExpr e) as


desugarExpr :: Expr -> CoreExpr
desugarExpr e = case e of

    If c t e -> CIf (desugarExpr c) (desugarExpr t) (desugarExpr e)

    Op op l r -> CApp (CApp (CVar (opName op)) (desugarExpr l)) (desugarExpr r)

    Lit l -> CLit l

    Str s -> CStr s

    BoolConst b -> CBool b

    Var v -> CVar v

    Lam n t e -> CLam n t (desugarExpr e)

    Funcall n as -> foldl (\n a -> CApp n (desugarExpr a)) (desugarExpr n) as

    Let n d e -> CLet n (desugarExpr d) (desugarExpr e)

    Block xs -> seqs xs
        where seqs [x]    = desugarExpr x
              seqs (x:xs) = desugarExpr x `CSeq` seqs xs


opName :: String -> String
opName op = case op of
                "==" -> "$__equal__$"
                "/=" -> "$__notequal_$"
                "<<" -> "$__shiftl__$"
                ">>" -> "$__shiftr__$"
                "&"  -> "$__and__$"
                "|"  -> "$__or__$"
                "^"  -> "$__xor__$"
                "<"  -> "$__lower__$"
                ">"  -> "$__greater__$"
                "+"  -> "$__add__$"
                "-"  -> "$__sub__$"
                "*"  -> "$__mult__$"
                "/"  -> "$__div__$"
                "%"  -> "$__mod__$"
