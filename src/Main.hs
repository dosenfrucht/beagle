module Main where

import Control.Monad

import Lexer
import Parser
import Codecheck
import Desugar
import Lifter
import Bytecode

main :: IO ()
main = do
    src <- readFile "example/test.b"
    let toks = lexer src
    case parse toks of
        Left err  -> putStrLn err
        Right res -> let res' = desugar res
                     in
                     case typecheck res' of
                         Left err -> putStrLn err
                         Right res'' -> do
                             let combs = lamLift res''
                                 code  = compile combs
                             forM_ code $ \(n, ic, c) -> do
                                 putStrLn n
                                 print ic
                                 forM_ c $ \i -> do
                                     putStr "    "
                                     print i
