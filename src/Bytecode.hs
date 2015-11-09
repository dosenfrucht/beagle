module Bytecode where

import Data.Word

data Instruction = PushNum Word64
                 | PushStr String
                 | PushBool Bool
                 | Branch [Instruction] [Instruction]
                 | TailCall String
                 | Call String
                 | Pop String
                 | Discard
                 | Load String
                 | Delete String
                 | DeleteAll

                 | Add
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
