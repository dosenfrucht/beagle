module Lifter (
    lamLift,
    SuperComb(..)
) where

import qualified Data.Set as S
import qualified Data.Map as M

import Control.Monad.RWS
import Data.Maybe
import Data.List

import Codecheck
import Syntax
                        --      frees  args
data SuperComb = SuperComb Name [Name] [Name] CoreExpr
               deriving (Eq)

instance Show SuperComb where
    show (SuperComb n fs as e) = n ++ "(" ++ intercalate ", " fs ++ ")("
                                   ++ intercalate ", " as ++ ") = "
                                   ++ show e ++ ";"



lamLift :: [CoreToplevel] -> [SuperComb]
lamLift cs = concatMap (lift' te) cs
    where te = typeEnv cs

lift' :: TypeEnv -> CoreToplevel -> [SuperComb]
lift' te (CTopVarDef n _ e) = SuperComb n [] (reverse args) e'' : ll
    where args        = map fst eArgs
          (eArgs, e') = removeTrailingLambdas e
          (e'', ll)   = liftExpr n te e'


removeTrailingLambdas :: CoreExpr -> ([(Name, Type)], CoreExpr)
removeTrailingLambdas = rm []
    where rm acc (CLam n t e) = rm ((n, t) : acc) e
          rm acc e            = (acc, e)


liftExpr :: String -> TypeEnv -> CoreExpr -> (CoreExpr, [SuperComb])
liftExpr s te e = let (e', _, s') = runRWS (l s e) te te
                  in (e', s')
    where l :: String
            -> CoreExpr
            -> RWS TypeEnv [SuperComb] TypeEnv CoreExpr
          l s e = case e of
                    CIf c t e -> do
                        c' <- l s c
                        t' <- l s t
                        e' <- l s e
                        pure $ CIf c' t' e'

                    CApp a b -> do
                        a' <- l s a
                        b' <- l s b
                        pure $ CApp a' b'

                    CLet n d e -> do
                        let s' = s ++ "_" ++ n
                        d' <- l s d
                        te <- get
                        let te' = extendManyTypeEnv te [(n, undefined)]
                        put te'
                        e' <- l s' e
                        put te
                        pure $ CLet n d' e'

                    CSeq a b -> do
                        a' <- l s a
                        b' <- l s b
                        pure $ CSeq a' b'

                    lam@(CLam n t e) -> do
                        te <- ask
                        let frees = S.toList $ free te lam
                            (eArgs, e') = removeTrailingLambdas lam
                            scomb = SuperComb (s ++ ":") frees (reverse $ map fst eArgs) e'
                        tell [scomb]
                        pure $ foldl CApp (CVar (s ++ ":")) (map CVar frees)

                    x -> pure x

free :: TypeEnv -> CoreExpr -> S.Set String
free te e = case e of
    CIf c t e ->  free te c
        `S.union` free te t
        `S.union` free te e

    CLit _ -> S.empty

    CStr _ -> S.empty

    CBool _ -> S.empty

    CVar s | isNothing $ getFromTypeEnv te s -> S.singleton s
           | otherwise                       -> S.empty

    CLam n t e -> n `S.delete` free te e

    CApp l r -> free te l `S.union` free te r

    CLet n d e -> n `S.delete` free te e

    CSeq l r-> free te l `S.union` free te r


extendManyTypeEnv :: TypeEnv -> [(Name, Type)] -> TypeEnv
extendManyTypeEnv te []          = te
extendManyTypeEnv te ((n, t):xs) = extendTypeEnv te n t



right :: Either a b -> b
right (Right b) = b
