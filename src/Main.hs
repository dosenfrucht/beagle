module Main where

import Lexer
import Parser
import Codecheck
import Desugar
import Lifter

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
                             mapM_ print $ lamLift res''
