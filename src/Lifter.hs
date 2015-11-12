module Lifter (
    lamLift,
    SuperComb(..)
) where

import qualified Data.Set as S
import qualified Data.Map as M

import Control.Monad.RWS
import Data.Maybe

import Codecheck
import Syntax

data SuperComb = SuperComb Name [Name] [Name] CoreExpr
               deriving (Show, Eq)



lamLift :: [CoreToplevel] -> [SuperComb]
lamLift cs = concatMap (lift' te) cs
    where te = typeEnv cs

lift' :: TypeEnv -> CoreToplevel -> [SuperComb]
lift' te (CTopVarDef n _ e) = SuperComb n freeVars (reverse args) e'' : ll
    where args        = map fst eArgs
          (eArgs, e') = removeTrailingLambdas e
          (e'', ll)   = liftExpr n te' e'
          freeVars    = S.toList $ free te e
          te'         = extendManyTypeEnv te eArgs


removeTrailingLambdas :: CoreExpr -> ([(Name, Type)], CoreExpr)
removeTrailingLambdas = rm []
    where rm acc (CLam n t e) = rm ((n, t) : acc) e
          rm acc e            = (acc, e)


liftExpr :: String -> TypeEnv -> CoreExpr -> (CoreExpr, [SuperComb])
liftExpr s te e = let (e', _, s') = runRWS (l s e) te ()
                  in (e', s')
    where l :: String
            -> CoreExpr
            -> RWS TypeEnv [SuperComb] () CoreExpr
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
                        e' <- l s' e
                        pure $ CLet n d' e'

                    CSeq a b -> do
                        a' <- l s a
                        b' <- l s b
                        pure $ CSeq a' b'

                    -- TODO Still needs a looooot of work. I suppose.
                    -- ¯\_(._.)_/¯
                    -- At least its something.
                    lam@(CLam n t e) -> do
                        te' <- ask
                        lam' <- l (s ++ ":") e
                        let frees = S.toList $ free te' lam
                            (eArgs, e') = removeTrailingLambdas lam
                            scomb = SuperComb (s ++ ":") frees (map fst eArgs) lam'
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

    CLam n t e -> free (extendTypeEnv te n t) e

    CApp l r -> free te l `S.union` free te r

    CLet n d e -> free te d `S.union`
                  free (extendTypeEnv te n (right $ getType te d)) e

    CSeq l r-> free te l `S.union` free te r


extendManyTypeEnv :: TypeEnv -> [(Name, Type)] -> TypeEnv
extendManyTypeEnv te []          = te
extendManyTypeEnv te ((n, t):xs) = extendTypeEnv te n t



right :: Either a b -> b
right (Right b) = b
