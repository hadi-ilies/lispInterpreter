module Main where

import System.Environment
import System.Exit
import Lib
    
main :: IO ()

cli = do
    name <- getLine
    putStrLn(name)
    if (name == "exit") then exitWith ExitSuccess
    else
        -- test
        cli

main = do
    (expr:_) <- getArgs
    putStrLn (readExpr expr)
    cli
    --getArgs >>= parse >>= putStr
-- parse ["-h"] = usage >> exit
-- parse ["-i"] = mode >> exit
-- parse []     = getContents
-- parse fs     = concat `fmap` mapM readFile fs
    
usage   = putStrLn "Usage: lisp [-i] [file ..]"
mode = putStrLn "Cli Mode"
exit    = exitWith ExitSuccess
die     = exitWith (ExitFailure 1)