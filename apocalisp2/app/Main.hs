module Main where

import System.Environment
import System.Exit
import Lib
    
main :: IO ()
    
main = getArgs >>= parse >>= putStr
parse ["-h"] = usage >> exit
parse ["-i"] = mode >> exit
parse []     = getContents
parse fs     = concat `fmap` mapM readFile fs
    
usage   = putStrLn "Usage: lisp [-i] [file ..]"
mode = putStrLn "Cli Mode"
exit    = exitWith ExitSuccess
die     = exitWith (ExitFailure 1)