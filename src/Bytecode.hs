module Bytecode (
    compile
) where

import Data.Monoid
import Data.Sequence (Seq, singleton, fromList)
import Data.Foldable hiding (concatMap, sum)
import Data.Word
import Data.List hiding (concatMap, sum)

import Syntax

-- actual bytecode instruction set
data Instruction = PushNum    Word64
                 | PushStr    String
                 | PushVar    Int    -- Offset im Stack
                 | PushGlobal String
                 | Call       Int    -- N Aufrufe
                 | Unwind     Int    -- N Variablen entfernen
                 | Pop
                 | Return
                 | Cond [Instruction] [Instruction]
                 deriving (Show)

instance Eq Instruction where
    PushNum    x == PushNum    y = x == y
    PushStr    x == PushStr    y = x == y
    PushVar    x == PushVar    y = x == y
    PushGlobal x == PushGlobal y = x == y
    Call _       == Call _       = True
    Unwind _     == Unwind _     = True
    Pop          == Pop          = True
    Return       == Return       = True
    Cond a b     == Cond x y     = a == x && b == y
    _            == _            = False

--                  Name, isCAF, Code
type CompiledFun = (Name, Bool, [Instruction])

compile :: [SuperComb] -> [CompiledFun]
compile = map compileSC

compileSC :: SuperComb -> CompiledFun
compileSC (SuperComb n fs as e) = (n, isCAF, simplify $ toList code)
    where code  = generateCode (reverse $ fs ++ as) e
                      <> fromList [Unwind . length $ fs ++ as, Return]
          isCAF = null $ fs ++ as


generateCode :: [Name] -> CoreExpr -> Seq Instruction
generateCode vs e = case e of

    CLit i -> singleton $ PushNum i

    CStr s -> singleton $ PushStr s

    CBool b -> singleton $ PushNum (if b then 1 else 0)

    CVar v -> singleton $ indexOf vs v

    CLet n d e -> generateCode vs d <> generateCode (n : vs) e
                                    <> singleton (Unwind 1)

    CApp l r -> generateCode vs r <> generateCode vs l <> singleton (Call 1)

    CSeq a b -> generateCode vs a <> singleton (Unwind 1) <> generateCode vs b

    CIf c t e -> generateCode vs c <> singleton (Cond
                                                    (toList $ generateCode vs t)
                                                    (toList $ generateCode vs e)
                                                )


indexOf :: [String] -> String -> Instruction
indexOf = index' 0
    where index' _ []       v = PushGlobal v
          index' i (x : xs) v
                  | x == v    = PushVar i
                  | otherwise = index' (i + 1) xs v




simplify :: [Instruction] -> [Instruction]
simplify code = concatMap flatten (group code)

flatten :: [Instruction] -> [Instruction]
flatten (Call i : xs) = [Call n]
    where n = sum $ map unCallOrUnUnwind (Call i : xs)
flatten (Unwind i : xs) = if n == 0 then [] else [Unwind n]
    where n = sum $ map unCallOrUnUnwind (Unwind i : xs)
flatten x = x



unCallOrUnUnwind :: Instruction -> Int
unCallOrUnUnwind (Call i)   = i
unCallOrUnUnwind (Unwind i) = i
