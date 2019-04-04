module Lib
    ( symbol,
      readExpr
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