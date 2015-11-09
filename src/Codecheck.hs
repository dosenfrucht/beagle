module Codecheck where

import qualified Data.Map as M

import Control.Monad

import Syntax

type TypeEnv = (TypeVarMap, TypeFunMap)
type TypeVarMap = M.Map String Type
type TypeFunMap = M.Map String ([Type], Type)


biFun :: String -> String -> String -> ([Type], Type)
biFun a b c = ([TyName a, TyName b], TyName c)


defVars :: TypeVarMap
defVars = M.empty

defFuns :: TypeFunMap
defFuns = M.fromList
    [
        ("+", biFun "num" "num" "num"),
        ("-", biFun "num" "num" "num"),
        ("*", biFun "num" "num" "num"),
        ("/", biFun "num" "num" "num"),
        ("%", biFun "num" "num" "num"),
        (">", biFun "num" "num" "bool"),
        ("<", biFun "num" "num" "bool"),
        ("==", biFun "num" "num" "bool"),
        ("/=", biFun "num" "num" "bool"),
        (">>", biFun "num" "num" "num"),
        ("<<", biFun "num" "num" "num"),
        ("&", biFun "num" "num" "num"),
        ("|", biFun "num" "num" "num"),
        ("^", biFun "num" "num" "num"),

        ("put_string", ([TyName "str"], TyName "num"))
    ]


check :: [Toplevel] -> Either String [Toplevel]
check ts = do
    let te@(tvm, tfm) = genTypeEnv ts
    check' ts te


check' :: [Toplevel] -> TypeEnv -> Either String [Toplevel]
check' [] _ = return []
check' (v@(TopVarDef n t e) : ts) te = do
    et <- getType e (defVars, snd te)
    if t == et
        then do
            rest <- check' ts te
            return $ v : rest
        else fail $ "Type of " ++ n ++ " does not match: expected " ++ show t ++
                       " but got " ++ show et

check' (f@(TopFunDef n args t e) : ts) te = do
    let te' = (M.union (fst te) (M.fromList args) , snd te)
    et <- getType e te'
    if t == et
        then do
            rest <- check' ts te
            return $ f : rest
        else fail $ "Type of " ++ n ++ " does not match: expected " ++ show t ++
                       " but got " ++ show et



getType :: Expr -> TypeEnv -> Either String Type
getType e te@(tv, tf) = case e of

    Lit _ -> return $ TyName "num"

    Str s -> return $ TyName "str"

    BoolConst _ -> return $ TyName "bool"

    Var v -> case M.lookup v tv of
                 Nothing -> fail $ "Unknown variable " ++ v
                 Just t  -> return t

    If c t e -> do
        c' <- getType c te
        t' <- getType t te
        e' <- getType e te
        if c' /= TyName "bool"
            then fail $ "expression " ++ show c ++ " must have type bool but "
                           ++ "is has type " ++ show c'
            else if t' /= e'
                    then fail $ "could not match type " ++ show t' ++ " with "
                                  ++ show e'
                    else return t'

    Let n d e -> do
        d' <- getType d te
        let te' = (M.union (fst te) (M.fromList [(n, d')]) , snd te)
        getType e te'

    Funcall n args -> do
        args' <- mapM (`getType` te) args
        case M.lookup n tf of
            Nothing         -> fail $ "function " ++ n ++ " is not defined"
            Just (eargs, t) ->
                if eargs /= args'
                    then fail $ "argument types do not match in call to " ++ n
                    else return t

    Op n l r -> getType (Funcall n [l, r]) te

    Block es -> do
        es' <- mapM (`getType` te) es
        return $ last es'







genTypeEnv :: [Toplevel] -> TypeEnv
genTypeEnv ts = (genTypeVar ts, genTypeFun ts)

genTypeVar :: [Toplevel] -> TypeVarMap
genTypeVar [] = defVars
genTypeVar (TopVarDef b t _ : ts) = tymap
    where tymap     = M.insert b t tymapRest
          tymapRest = genTypeVar ts
genTypeVar (_ : ts) = genTypeVar ts

genTypeFun :: [Toplevel] -> TypeFunMap
genTypeFun [] = defFuns
genTypeFun (TopFunDef b ta t _ : ts) = tymap
    where tymap     = M.insert b (map snd ta, t) tymapRest
          tymapRest = genTypeFun ts
genTypeFun (_ : ts) = genTypeFun ts
