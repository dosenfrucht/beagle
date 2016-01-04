module Codecheck (
    typecheck,
    typeEnv,
    TypeEnv,
    extendTypeEnv,
    getFromTypeEnv,
    getType
) where

import qualified Data.Map as M

import Control.Monad
import Control.Applicative

import Syntax


type TypeEnv = M.Map String Type

defaultTypes :: TypeEnv
defaultTypes = M.fromList
    [
    ("$__add__$", biFun "num" "num" "num"),
    ("$__sub__$", biFun "num" "num" "num"),
    ("$__mult__$", biFun "num" "num" "num"),
    ("$__div__$", biFun "num" "num" "num"),
    ("$__mod__$", biFun "num" "num" "num"),
    ("$__greater__$", biFun "num" "num" "bool"),
    ("$__lower__$", biFun "num" "num" "bool"),
    ("$__equal__$", biFun "num" "num" "bool"),
    ("$__notequal_$", biFun "num" "num" "bool"),
    ("$__shiftr__$", biFun "num" "num" "num"),
    ("$__shiftl__$", biFun "num" "num" "num"),
    ("$__and__$", biFun "num" "num" "num"),
    ("$__or__$", biFun "num" "num" "num"),
    ("$__xor__$", biFun "num" "num" "num")
    ]

biFun :: String -> String -> String -> Type
biFun a b c = TyFun (TyName a) (TyFun (TyName b) (TyName c))

extendTypeEnv :: TypeEnv -> String -> Type -> TypeEnv
extendTypeEnv te n t = M.insert n t te

getFromTypeEnv :: TypeEnv -> String -> Maybe Type
getFromTypeEnv te s = M.lookup s te

typecheck :: [CoreToplevel] -> Either String [CoreToplevel]
typecheck ts = checkAll (typeEnv ts) ts


checkAll :: TypeEnv -> [CoreToplevel] -> Either String [CoreToplevel]
checkAll te []                      = pure []
checkAll te (CTopVarDef n t e : xs) = do
    e' <- getType te e
    if t == e'
        then do
            rest <- checkAll te xs
            pure $ CTopVarDef n t e : rest
        else
            fail $ "Types of " ++ n ++ " do not match.\n"
                ++ "expected: " ++ show t ++ "\n"
                ++ "got:      " ++ show e'

typeEnv :: [CoreToplevel] -> TypeEnv
typeEnv []    = defaultTypes
typeEnv (CTopVarDef n t _ :xs) = M.insert n t (typeEnv xs)



getType :: TypeEnv -> CoreExpr -> Either String Type
getType te e = case e of

    CLit _ -> pure $ TyName "num"

    CStr _ -> pure $ TyName "str"

    CBool _ -> pure $ TyName "bool"

    CIf c t e -> do
        c' <- getType te c
        t' <- getType te t
        e' <- getType te e
        if c' /= TyName "bool"
            then fail $ "Expression " ++ show c ++ " must have type bool but "
                           ++ "is has type " ++ show c'
            else if t' /= e'
                    then fail $ "Could not match type " ++ show t' ++ " with "
                                  ++ show e'
                    else pure t'

    CVar n -> case M.lookup n te of
                Nothing -> fail $ "Unknown identifier: " ++ n
                Just t  -> pure t

    CLam n t e -> do
        e' <- getType (extendTypeEnv te n t) e
        pure $ TyFun t e'

    CApp l r -> do
        l' <- getType te l
        r' <- getType te r
        case l' of
            TyFun tx te | tx == r' -> pure te
            _                      -> fail $ "Cannot call\n     " ++ show l'
                                          ++ "\nwith " ++ show r'

    CLet n d e -> do
        d' <- getType te d
        e' <- getType (extendTypeEnv te n d') e
        pure e'

    CSeq l r -> do
        getType te l
        getType te r
