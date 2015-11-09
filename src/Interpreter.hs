module Interpreter where

import qualified Data.Map as M

import Data.Word
import Data.Bits

import Bytecode

type Prog = M.Map String [Instruction]

type Stack = [Val]
type Heap  = M.Map String Val
type Locals = Heap

type Mem = (Stack, Heap, Locals)

data Val = ValNum  Word64
         | ValStr  String
         | ValBool Bool
         deriving (Show, Eq)

runProg :: Prog -> IO Val
runProg p = do
    (_, heap, _) <- eval p "::init_vars" ([], M.empty, M.empty)
    (s:_, _, _) <- eval p "main" ([], heap, M.empty)
    return s

eval :: Prog -> String -> Mem -> IO Mem
eval p f (s, h, l) = case M.lookup f p of
    Nothing -> fail $ "Function " ++ f ++ " is not defined"
    Just r  -> evalExpr p r (s, h, l)


evalExpr :: Prog -> [Instruction] -> Mem -> IO Mem
evalExpr p [] (s, h, l) = return (s, h, l)
evalExpr p (x:xs) (s, h, l) = do
    print s
    newMem@(s, _, l) <- evalInstr p x (s, h, l)
    evalExpr p xs newMem


evalInstr :: Prog -> Instruction -> Mem -> IO Mem
evalInstr p (PushNum x)  (s, h, l) = return (ValNum  x : s, h, l)
evalInstr p (PushStr x)  (s, h, l) = return (ValStr  x : s, h, l)
evalInstr p (PushBool x) (s, h, l) = return (ValBool x : s, h, l)
evalInstr p (Branch t e) (ValBool True: s, h, l) = evalExpr p t (s, h, l)
evalInstr p (Branch t e) (_           : s, h, l) = evalExpr p e (s, h, l)
evalInstr p (TailCall x) (s, h, _) = do
    (s', h', _) <- eval p x (s, h, M.empty)
    return (s', h', l)
evalInstr p (Call x) (s, h, l) = do
    (s', h', _) <- eval p x (s, h, M.empty)
    return (s', h', l)
evalInstr p (Pop x)  (s : ss, h, l) =
    return (ss, h, M.insert x s l)
evalInstr p  Discard (_ : ss, h, l) = return (ss, h, l)
evalInstr p (Load x) (s, h, l) = case M.lookup x l of
    Nothing -> case M.lookup x h of
                    Nothing -> fail $ "No such var " ++ x
                    Just x' -> return (x' : s, h, l)
    Just x' -> return (x' : s, h, l)
evalInstr p (Delete x) (s, h, l) = return (s, h, M.delete x l)
evalInstr p  DeleteAll (s, h, l) = return (s, h, M.empty)


evalInstr p Add (ValNum a : ValNum b : s, h, l) =
    return (ValNum (a + b) : s, h, l)
evalInstr p Sub (ValNum a : ValNum b : s, h, l) =
    return (ValNum (a - b) : s, h, l)
evalInstr p Mul (ValNum a : ValNum b : s, h, l) =
    return (ValNum (a * b) : s, h, l)
evalInstr p Div (ValNum a : ValNum b : s, h, l) =
    return (ValNum (div a b) : s, h, l)
evalInstr p Mod (ValNum a : ValNum b : s, h, l) =
        return (ValNum (mod a b) : s, h, l)
evalInstr p Lt (ValNum a : ValNum b : s, h, l) =
    return (ValBool (a < b) : s, h, l)
evalInstr p Gt (ValNum a : ValNum b : s, h, l) =
    return (ValBool (a > b) : s, h, l)
evalInstr p Eq (ValNum a : ValNum b : s, h, l) =
    return (ValBool (a == b) : s, h, l)
evalInstr p Neq (ValNum a : ValNum b : s, h, l) =
    return (ValBool (a /= b) : s, h, l)
evalInstr p Shr (ValNum a:ValNum b: s, h, l) =
    return (ValNum (shiftR a (wti b)) : s, h, l)
evalInstr p Shl (ValNum a:ValNum b: s, h, l) =
    return (ValNum (shiftL a (wti b)) : s, h, l)
evalInstr p And (ValNum a:ValNum b: s, h, l) =
    return (ValNum (a .&. b) : s, h, l)
evalInstr p Or (ValNum a:ValNum b: s, h, l) =
    return (ValNum (a .|. b) : s, h, l)
evalInstr p Xor (ValNum a:ValNum b: s, h, l) =
    return (ValNum (xor a b) : s, h, l)




wti :: Word64 -> Int
wti = fromInteger . toInteger
