module Compiler where

import qualified Data.Map as M

import Data.List

import Syntax
import Bytecode

compile :: [Toplevel] -> M.Map String [Instruction]
compile ts = compileVars vars `M.union` compileFuns funs
    where vars = foldr stepVar [] ts
          stepVar (TopVarDef n _ e) acc = (n, e) : acc
          stepVar _                 acc = acc
          funs = foldr stepFun [] ts
          stepFun (TopFunDef n as _ e) acc = (n, map fst as, e) : acc
          stepFun _                     acc = acc




compileVars :: [(Name, Expr)] -> M.Map String [Instruction]
compileVars ts = M.singleton "::init_vars" initCode `M.union` varFuns
    where initCode = compileVars' $ map fst ts
          varFuns  = compileFuns (map (\(n, e) -> ("::init_" ++ n, [], e)) ts)
          compileVars' = foldr (\x -> (++) [Call $ "::init_" ++ x, Pop x]) []


compileFuns :: [(Name, [Name], Expr)] -> M.Map String [Instruction]
compileFuns [] = M.empty
compileFuns ((n, as, e):fs) = M.insert n instrs $ compileFuns fs
    where instrs = popArgs as ++ compileExpr e True ++ deleteVars as
          popArgs = map Pop
          deleteVars = map Delete

-- Expr to be compiled,
-- Bool tail call optimisation?
compileExpr :: Expr -> Bool -> [Instruction]
compileExpr e b = case e of

    Lit w -> [PushNum w]

    Str s -> [PushStr s]

    BoolConst b -> [PushBool b]

    Var v -> [Load v]

    Funcall n as | b ->
                    let ase = concat . reverse $ map (`compileExpr` False) as
                    in ase ++ [DeleteAll, TailCall n]
                 | otherwise ->
                    let ase = concat . reverse $ map (`compileExpr` False) as
                    in ase ++ [Call n]


    Let n d e ->
        compileExpr d False
        ++
        [Pop n]
        ++
        compileExpr e b
        ++
        [Delete n]

    Block es ->
        let ess = map (`compileExpr` False) es
        in intercalate [Discard] ess

    Op n l r ->
        compileExpr r False
        ++
        compileExpr l False
        ++
        [operatorInstr n]

    If c t e ->
        compileExpr c False
        ++
        [Branch (compileExpr t b) (compileExpr e b)]


operatorInstr :: String -> Instruction
operatorInstr n = m M.! n
    where m = M.fromList
            [
                ("+", Add),
                ("-", Sub),
                ("*", Mul),
                ("/", Div),
                ("%", Mod),
                ("<", Lt),
                (">", Gt),
                ("==", Eq),
                ("/=", Neq),
                (">>", Shr),
                ("<<", Shl),
                ("&", And),
                ("|", Or),
                ("^", Xor)
            ]
