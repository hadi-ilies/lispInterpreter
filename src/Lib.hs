module Lib
    ( readExpr,
      asList,
      eval,
      display
    ) where

import Text.ParserCombinators.Parsec
import Pars

skipSpace :: Parser ()
skipSpace = skipMany1 space 

-- val@(type...) matches against any LispVal that's a string and then binds val to the whole LispVal
eval :: LispVal -> LispVal
eval val@(String _) = val
eval val@(Number _) = val
eval val@(Bool _) = val
eval (List [Atom "quote", val]) = val
eval (List [Atom "\'", val]) = val

-- unwords list to string
unwordsList :: [LispVal] -> String
unwordsList = unwords . map display

-- show func convert anything of his class to a String 
display :: LispVal -> String
display (String command) = command 
display (Atom command) = command
display (Number command) = show command
display (Bool True) = "#t"
display (Bool False) = "#f" 
display (List contents) = "(" ++ unwordsList contents ++ ")"
display (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ display tail ++ ")"

--NOTE f (g x) ==  f $ g x 
readExpr :: String -> LispVal
readExpr input = case parse parseExpr "lisp" input of
    Left err -> String ("No match: " ++ show err)
    Right val -> val  

-- transform a list of string to a string
-- NOTE : "'" is used to indicate a strict variant of a function
asList :: [String] -> String
asList ss = asList' ss where
asList' (a:b:ss) = a ++ (asList' (b:ss))
asList' (a:ss)   = a ++ asList' (ss)
asList' []       = ""