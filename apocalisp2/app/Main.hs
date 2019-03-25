module Main where

import System.Environment
import System.Exit
import Lib
    
main :: IO ()
    
main = getArgs >>= parse >>= putStr . tac
    
tac  = unlines . reverse . lines
    
parse ["-h"] = usage   >> exit
parse ["-i"] = mode >> exit
parse []     = getContents
parse fs     = concat `fmap` mapM readFile fs
    
usage   = putStrLn "Usage: tac [-vh] [file ..]"
mode = putStrLn "Cli Mode"
exit    = exitWith ExitSuccess
die     = exitWith (ExitFailure 1)