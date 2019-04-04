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

hadiPls :: [String] -> Bool
hadiPls [] = False
hadiPls (a:as) = if (a == "-i") then True else hadiPls (as) 

main = do
    expr <- getArgs
    if (expr == []) then cli
    else if (hadiPls (expr) == True) then cli else putStrLn ("LOL you have to read files")
    --getArgs >>= parse >>= putStr
-- parse ["-h"] = usage >> exit
-- parse ["-i"] = mode >> exit
-- parse []     = getContents
-- parse fs     = concat `fmap` mapM readFile fs
    
usage   = putStrLn "Usage: lisp [-i] [file ..]"
mode = putStrLn "Cli Mode"
exit    = exitWith ExitSuccess
die     = exitWith (ExitFailure 1)