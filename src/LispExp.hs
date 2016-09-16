module LispExp where

import Data.Vector hiding ((++), map, foldl1, null, mapM, all, zipWith, tail)

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Integer Integer
             | Float Float
             | String String
             | Bool Bool
             | Character Char
             | Quasiquote [QuasiquoteVal]
             | Vector (Vector LispVal)
             deriving (Eq)

data QuasiquoteVal = LispVal LispVal
                   | Unquoted LispVal
                   | UnquotedList LispVal
                   deriving (Show, Eq)

instance Show LispVal where
  show (Atom name) = name
  show (String contents) = "\"" ++ contents ++ "\""
  show (Integer content) = show content
  show (Float content) = show content
  show (Bool True) = "#t"
  show (Bool False) = "#f"
  show (List contents) = "(" ++ unwordsList contents  ++ ")"
  show (DottedList head tail) =
    "(" ++ unwordsList head ++ " . " ++ show tail ++ ")"
  show (Character c) = show c -- @TODO
  show (Vector v) =
    let content = Data.Vector.foldl (\acc a -> acc ++ " " ++ show a) "" v
    in "#(" ++ content ++ ")"

unwordsList :: [LispVal] -> String
unwordsList = unwords . map show

