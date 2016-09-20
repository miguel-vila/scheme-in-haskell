module Parser where

import LispExp
import Text.ParserCombinators.Parsec hiding (spaces)
import LispError
import Data.Vector (fromList)
import Numeric (readFloat, readHex, readOct, readDec)

readOrThrow :: Parser a -> String -> ThrowsError a
readOrThrow parser input = case parse parser "lisp" input of
  Left err -> throwError_ $ Parser err
  Right val -> return val

readExpr :: String -> ThrowsError LispVal
readExpr = readOrThrow parseExpr

readExprList :: String -> ThrowsError [LispVal]
readExprList = readOrThrow ( sepEndBy (many comment >> parseExpr) (spaces <|> (char '\n' >> return ())) )

comment :: Parser ()
comment = do
  char ';'
  manyTill anyChar (char '\n')
  return ()

symbols = "!$%&|*+-/:<=?>@^_#"

symbol :: Parser Char
symbol = oneOf symbols

spaces :: Parser ()
spaces = skipMany1 space

escapeChars :: Parser Char
escapeChars =
  (char '\\' >> char '"' >> (return '"'))
  <|> (char '\\' >> char 'n' >> (return '\n'))
  <|> (char '\\' >> char 'r' >> (return '\r'))
  <|> (char '\\' >> char 't' >> (return '\t'))
  <|> (char '\\' >> char '\\' >> (return '\\'))

parseString :: Parser LispVal
parseString = do
  char '"'
  str <- many $ escapeChars <|> (noneOf "\"")
  char '"'
  return $ String str

parseAtom :: Parser LispVal
parseAtom = do
  f <- letter <|> symbol
  rest <- many (letter <|> symbol <|> digit)
  let atom = f : rest
  return $ case atom of
    "#t" -> Bool True
    "#f" -> Bool False
    otherwise -> Atom atom

parseInteger :: Parser LispVal
parseInteger = do
  s <- getInput
  case readDec s of
    [(n,'.':_)] ->
      fail "decimal"
    [(n,s')] ->
      (Integer n) <$ setInput s'
    _ -> fail "Not an integer"

parseFloat :: Parser LispVal
parseFloat = do
  s <- getInput
  case readFloat s of
    [(n,s')] -> (Float n) <$ setInput s'
    _ -> fail "Not a float"

parseHex :: Parser LispVal
parseHex = do
  char '#'
  char 'x'
  hex <- many1 (digit <|> oneOf "abcdef")
  case readHex hex of
    [(n,s)] -> (Integer n) <$ setInput s
    _ -> fail "Not a valid hex"

parseOct :: Parser LispVal
parseOct = do
  char '#'
  char 'o'
  oct <- many1 digit
  case readOct oct of
    [(n,s)] -> (Integer n) <$ setInput s
    _ -> fail "Not a valid oct"

parseChar :: Parser LispVal
parseChar =
  let keywords = [ ("space" , ' ') , ("newline" , '\n') ]
      keywordParses = map (\(keyword,c) -> (string keyword) >> (return c)) keywords
      parseKeyword = choice keywordParses
  in do
    char '#'
    char '\\'
    c <- anyChar <|> parseKeyword
    return $ Character c

parseNumeric :: Parser LispVal
parseNumeric =
  parseHex
  <|> parseOct
  <|> parseInteger
  <|> parseFloat

parseExpr :: Parser LispVal
parseExpr =
  try parseVector
  <|> try parseChar
  <|> try parseNumeric
  <|> parseString
  <|> parseAtom
  <|> parseQuoted
  <|> do char '('
         x <- (try parseList) <|> parseDottedList
         char ')'
         return x
--  <|> parseQuasiquote
--  <|> parseVector


parseList :: Parser LispVal
parseList = List <$> parseExpr `sepEndBy` spaces

parseDottedList :: Parser LispVal
parseDottedList = do
  head <- endBy parseExpr spaces
  tail <- char '.' >> spaces >> parseExpr
  return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = do
  char '\''
  x <- parseExpr
  return $ List [Atom "quote", x]

parseUnquoted :: Parser QuasiquoteVal
parseUnquoted = do
  char ','
  Unquoted <$> parseExpr

parseUnquotedList :: Parser QuasiquoteVal
parseUnquotedList = do
  char ','
  char '@'
  UnquotedList <$> parseExpr

parseQuasiquote :: Parser LispVal
parseQuasiquote = do
  char '`'
  char '('
  quasiquoteVals <- sepBy (parseUnquoted <|> parseUnquotedList <|> (LispVal <$> parseExpr)) spaces
  char ')'
  return $ Quasiquote quasiquoteVals

parseVector :: Parser LispVal
parseVector = do
  char '#'
  char '('
  contents <- sepBy parseExpr spaces
  char ')'
  return $ Vector $ fromList contents
