module Lib
    ( readExpr,
      asList
    ) where

import Text.ParserCombinators.Parsec
import Pars

skipSpace :: Parser ()
skipSpace = skipMany1 space 

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value"   

-- transform a list of string to a string
-- NOTE : "'" is used to indicate a strict variant of a function
asList :: [String] -> String
asList ss = asList' ss where
asList' (a:b:ss) = a ++ (asList' (b:ss))
asList' (a:ss)   = a ++ asList' (ss)
asList' []       = ""