module LispExp where

import Data.Vector hiding ((++), map, foldl1, null, mapM, all, zipWith, tail, and)
import Text.ParserCombinators.Parsec.Error
import Data.IORef

type Env = IORef [(String, IORef LispVal)] -- @TODO definition copied from Env.hs to avoid circular deps

type ThrowsError = Either LispError

data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String
               deriving (Eq)

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
             | PrimitiveFunc ([LispVal] -> ThrowsError LispVal)
             | Func { params :: [String]
                    , vararg :: (Maybe String)
                    , body :: [LispVal]
                    , closure :: Env
                    }

instance Eq LispVal where
  (Atom s1) == (Atom s2) = s1 == s2
  (List xs) == (List ys) = and $ zipWith (==) xs ys
  (DottedList xs x) == (DottedList ys y) = (and $ zipWith (==) xs ys) && (x == y)
  (Integer x) == (Integer y) = x == y
  (Float x) == (Float y) = x == y
  (String s1) == (String s2) = s1 == s2
  (Bool b1) == (Bool b2) = b1 == b2
  (Quasiquote xs) == (Quasiquote ys) = xs == ys
  (Vector v1) == (Vector v2) = v1 == v2
  (Func _ _ _ _) == (Func _ _ _ _) = False
  _ == _ = False

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
  show (PrimitiveFunc _) = "<primitive>"
  show ( Func { params = args
              , vararg = varargs
              , body = body
              , closure = env } ) =
    "(lambda (" ++ (unwords $ map show args) ++ (maybe "" (" . " ++) varargs) ++ ") ...)"

unwordsList :: [LispVal] -> String
unwordsList = unwords . map show

