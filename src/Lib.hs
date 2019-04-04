module Lib
    ( symbol,
      readExpr,
      asList
    ) where

import Text.ParserCombinators.Parsec

symbol :: Parser Char
symbol = oneOf "()!#$%&|*+-/:<=>?@^_~"

skipSpace :: Parser ()
skipSpace = skipMany1 space 

readExpr :: String -> String
readExpr input = case parse (skipSpace >> symbol) "lisp" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value"   

-- transform a list of string to a string
-- NOTE : "'" is used to indicate a strict variant of a function
asList :: [String] -> String
asList ss = asList' ss where
asList' (a:b:ss) = a ++ (',' : asList' (b:ss))
asList' (a:ss)   = a ++ asList' (ss)
asList' []       = ""