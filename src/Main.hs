module Main where

import Control.Monad
import System.IO

import Lexer
import Parser
import Codecheck
import Desugar
import Lifter
import Bytecode
import Codegen

main :: IO ()
main = do
    src <- readFile "example/test.b"

    h <- openFile "example/test.c" WriteMode

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
                                 cfuns = map cGen code
                             rc <- runtimeCode
                             rm <- runtimeMain
                             hPutStrLn h rc
                             forM_ (map fst cfuns) $ \l ->
                                 forM_ l $ hPutStrLn h
                             forM_ (map snd cfuns) $ hPutStrLn h

                             hPutStrLn h rm

                             hFlush h
                             hClose h
