module Main where

import Control.Monad
import System.IO
import System.Environment
import System.Exit
import System.FilePath

import Lexer
import Parser
import Codecheck
import Desugar
import Lifter
import Bytecode
import Codegen

main :: IO ()
main = do

    args' <- getArgs

    let args  = parseArgs args'

        inputFile = getInputFile args
        outputFile = getOutputFile args
        fc = getCompFlag args
        mem = getMemFlag args
        ss = getStackSize args

    when (inputFile == Nothing) $ do
        hPutStrLn stderr "No input file given"
        exitFailure

    let (Just inputFile') = inputFile
        outputFile'       = outputFileName inputFile' outputFile

    src <- readFile inputFile'

    h <- openFile outputFile' WriteMode

    when fc  $ hPutStrLn h "#define FORCOMPUTER"
    when mem $ hPutStrLn h "#define FREEARGS"

    case ss of
        Nothing -> return ()
        Just s  -> hPutStrLn h $ "#define STACKSIZE " ++ s

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



data Args = Args {
    getInputFile :: Maybe String,
    getOutputFile :: Maybe String,
    getCompFlag :: Bool,
    getMemFlag :: Bool,
    getStackSize :: Maybe String
}

parseArgs :: [String] -> Args
parseArgs = parse' (Args Nothing Nothing False False Nothing)
    where parse' args ("-o"      : fn : xs) = parse' (args { getOutputFile = Just fn}) xs
          parse' args ("--comp"       : xs) = parse' (args { getCompFlag = True}) xs
          parse' args ("--free-args"  : xs) = parse' (args { getMemFlag = True}) xs
          parse' args ("--stack-size" : s : xs) = parse' (args { getStackSize = Just s}) xs
          parse' args (fn             : xs) = parse' (args { getInputFile = Just fn}) xs
          parse' args []               = args


outputFileName :: String -> Maybe String -> String
outputFileName infile Nothing  = takeBaseName infile ++ ".c"
outputFileName _      (Just f) = f
