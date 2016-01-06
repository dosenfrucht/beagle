module Codegen(
    cGen,
    runtimeCode,
    runtimeMain
) where

import Bytecode
import Data.List

type CCode = String
type Prototype = String


runtimeCode :: IO String
runtimeCode = readFile "lib/runtime.c"


runtimeMain :: IO String
runtimeMain = readFile "lib/runtime_main.c"



cGen :: CompiledFun -> ([Prototype], CCode)
cGen (n, ic, na, code) = (protos, glob_val ++ fun')
    where fun = "uint8_t __fun_" ++ n' ++ "(uint8_t n)\n{\n\t" ++ varCheck ++ ccode ++ "\n}\n"
          glob_val = "struct __val " ++ n' ++ "()\n{\n\t" ++ str ++ "\n}\n"

          struct =  "struct __val v = {};\n\t"
                 ++ "v.data     = __fun_" ++ n' ++ ";\n\t"
                 ++ "return v;"

          struct' = ccode ++ "\n\t"
                 ++ "struct __val v = __read_val(0);\n\t"
                 ++ "__pop();\n\t"
                 ++ "return v;"

          protos = protoVal : if not ic then [protoFun] else []

          protoVal = "struct __val " ++ n' ++ "();"
          protoFun = "uint8_t __fun_" ++ n' ++ "(uint8_t n);"

          varCheck = "if (n != " ++ show na ++ ") {\n\t\treturn 0;\n\t}\n\t"

          str = if ic then struct' else struct

          fun' = if ic then "" else fun

          n'     = mangle n
          ccode  =  intercalate "\n\t" instrs
          instrs = map genInstr code

genInstr :: Instruction -> String
genInstr i = case i of

    PushNum i -> "__push_num(" ++ show i ++ ");"

    PushStr s -> "__push_str(" ++ show s ++ ");"

    PushVar i -> "__push_val(__read_val(" ++ show i ++ "));"

    PushGlobal g -> "__push_val(" ++ mangle g ++ "());"

    Call n -> "__call(" ++ show n ++ ");"

    Unwind n -> "__unwind(" ++ show n ++ ");"

    Pop -> "__pop();"

    Return -> "return 1;"

    Cond t e -> "if (__pop_num(0)) {" ++ tc ++ "} else {" ++ ec ++ "}"
        where tc = unwords (map genInstr t)
              ec = unwords (map genInstr e)

mangle :: String -> String
mangle s | head s == '$' = "builtin" ++ tail (init s) ++ "builtin"
mangle s = "b_" ++ concatMap mangleChar s ++ "_b"

mangleChar :: Char -> String
mangleChar c = case c of

    '$' -> "builtin"

    ':' -> "_lambda_"

    '\'' -> "_prime_"

    x   -> [x]
