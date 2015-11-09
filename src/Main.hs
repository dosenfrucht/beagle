module Main where

import Lexer
import Parser
import Codecheck
import Compiler
import Interpreter

main :: IO ()
main = do
    src <- readFile "example/test.b"
    let toks = lexer src
    case parse toks of
        Left err  -> putStrLn err
        Right res -> case check res of
            Left err -> putStrLn err
            Right res' -> let c = compile res'
                          in do print c
                                res <- runProg c
                                print res
