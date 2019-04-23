module Pars
    ( 
    parseExpr,
    LispVal (..)
    ) where
import Text.ParserCombinators.Parsec
import Prelude hiding (head, tail)
import Control.Monad --liftM
--import System.Environment

data LispVal = Atom String
        | List [LispVal]
        | DottedList [LispVal] LispVal
        | Number Integer
        | String String
        | Bool Bool
        | Float Float

parseString :: Parser LispVal
parseString = do
    _ <- char '"'
    x <- many (noneOf "\"")
    _ <- char '"'
    return $ String x

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"
    
parseAtom :: Parser LispVal
parseAtom = do 
    first <- letter <|> symbol
    rest <- many (letter <|> digit <|> symbol)
    let atom = first:rest
    return $ case atom of 
        "#t" -> Bool True
        "#f" -> Bool False
        _ -> Atom atom

parseNumber :: Parser LispVal
parseNumber = do
    fmap (Number . read) $ many1 digit

parseQuoted :: Parser LispVal
parseQuoted = do
    _ <- char '\''
    x <- parseExpr
    return $ List [Atom "quote", x]

parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
    head <- endBy parseExpr spaces
    tail <- char '.' >> spaces >> parseExpr
    return $ DottedList head tail

--'<|>' its just like a 'or' in python
parseExpr :: Parser LispVal
parseExpr = parseAtom
    <|> parseString
    <|> parseNumber
    <|> parseQuoted
    <|> do  
            _ <- char '('
            x <- try parseList <|> parseDottedList
            _ <- char ')'
            return x
