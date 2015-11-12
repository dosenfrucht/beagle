module Bytecode where

import Data.Word

data Instruction =
                   Add
                 | Sub
                 | Mul
                 | Div
                 | Mod
                 | Lt
                 | Gt
                 | Eq
                 | Neq
                 | Shr
                 | Shl
                 | And
                 | Or
                 | Xor
    deriving (Show, Eq)
