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

-- The function will return the corresponding value as (Just value), or Nothing if the key isn't in the map. 
-- NOTE: maybe is used to say that you allow the func to return nothing 
apply :: String -> [LispVal] -> LispVal
apply func args = maybe (Bool False) ($ args) $ lookup func primitives

--my dictionary
primitives :: [(String, [LispVal] -> LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem),
              ("cons", cons),
              ("car", car),
              ("cdr", cdr)]

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinop op params = Number (foldl1 op (map unpackNum params))

unpackNum :: LispVal -> Integer
unpackNum (Number n) = n
unpackNum (String n) = let parsed = reads n :: [(Integer, String)] in 
  if null parsed then 0
  else fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n
unpackNum _ = 0

-- val@(type...) matches against any LispVal that's a string and then binds val to the whole LispVal
eval :: LispVal -> LispVal
eval val@(String _) = val
eval val@(Number _) = val
eval val@(Bool _) = val 
eval (List [Atom "quote", val]) = val
eval (List [Atom "\'", val]) = val
eval (List (Atom func : args)) = apply func (map eval args)


--TODO cons function have to create error management
cons :: [LispVal] -> LispVal
cons [test] =  List []
cons [x1, List []] = List [x1]
cons [x, List xs] = List $ x : xs
cons [x, DottedList xs xlast] = DottedList (x : xs) xlast
cons [x1, x2] = DottedList [x1] x2
cons badArgList = List (badArgList)

--TODO car function have to create error managment
car :: [LispVal] -> LispVal
car [List (x : xs)]         = x
car [DottedList (x : xs) _] = x
car [badArg]                = List []
car badArgList = List badArgList

--TODO cdr function have to had error managment
cdr :: [LispVal] -> LispVal
cdr [List (x : xs)]         = List xs
cdr [DottedList [_] x]      =  x
cdr [DottedList (_ : xs) x] =  DottedList xs x
cdr [badArg]                =  List [badArg]
cdr badArgList              =  List badArgList

--cons function


-- unwords list to string
unwordsList :: [LispVal] -> String
unwordsList = unwords . map display

-- show func ,convert anything of his class to a String 
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