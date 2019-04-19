module Main where

import System.Environment
import System.Exit
import Lib
import Text.Regex.PCRE

main :: IO ()

cutWhitespace :: [String] -> [String]
cutWhitespace (x:xs) = (filter (\xs -> (xs /=' ')) x) : xs

--browse a string and send command to the Parser
execCommand :: String -> IO()
--execCommand lol = print (display (eval ( readExpr (lol))))
execCommand [] = putStrLn ("END of File Commands")
execCommand (x:xs) = do 
            --print xs
            if (x == '(') then do 
                print (display (eval ( readExpr ([x] ++ xs))))
                execCommand (xs)
            else execCommand (xs)

-- this function is an infinite loop which wait a string and will execute it loop is stoped when exit string was inserted 
cli :: Bool -> [String] -> IO()
cli True [] =  do
    putStrLn("Cli mode")
    command <- getLine
    --print(show readExpr command)
    --putStrLn(name)
    if (command == "exit") then exitWith ExitSuccess
    else do
        print (display (eval ( readExpr (command))))
        cli True []
cli True (a:as) = do
    putStrLn("Cli mode with files")
    fileContent <- readFile (a)
    let linesOfFiles = lines fileContent
    let commands = asList linesOfFiles
    if (commands == "") then
        cli True []
    else do
        execCommand commands
       -- print (commands)
       -- print (display (eval ( readExpr ( commands)))) 
        cli True as
cli False (a:as) = do
    putStrLn("Without Cli mode with files")
    fileContent <- readFile (a)
    --putStrLn(fileContent)
    let linesOfFiles = lines fileContent
    let commands = asList linesOfFiles
    execCommand commands
    --print (commands)
    --print (display (eval ( readExpr ( commands))))
    --print(asList linesOfFiles)
    if (as == []) then do
        putStrLn("Interactive Mode have been Stoped")
        exitWith ExitSuccess
    else
        --test
        cli False as

-- this function browse a list of strings and search the flag puted in parameter 
findFlag :: String -> [String] -> Bool
findFlag b [] = False
findFlag b (a:as) = if (a == b) then True else findFlag b as 

-- this function browse a list of string and return a list of string which has matched with my regex
findFile :: Regex -> [String] -> [String] 
findFile r [] = []
findFile r (a:as) = if (matchTest r a == True) then 
    [a] ++ findFile r as
    else findFile r as

--main funtion
main = do
    let regex = makeRegex "\\.(lisp|scm)$" :: Regex
    expr <- getArgs
    let files = findFile regex expr :: [String] 
    if (expr == []) then cli True []
    else if (files /= []) then do
        putStrLn $ show files
        putStrLn("Files have been found")
    else
        putStrLn("ERROR: Files haven't been found")
    if (findFlag "-i" expr == True) then cli True files else cli False files
    

--getArgs >>= parse >>= putStr
-- parse ["-h"] = usage >> exit
-- parse ["-i"] = mode >> exit
-- parse []     = getContents
-- parse fs     = concat `fmap` mapM readFile fs
    
usage   = putStrLn "Usage: lisp [-i] [file ..]"
mode = putStrLn "Cli Mode"
exit    = exitWith ExitSuccess
die     = exitWith (ExitFailure 1)