module StringOps where

import LispExp
import LispError
import BinaryOps
import Unpacking

strBoolBinop :: String -> (String -> String -> Bool) -> [LispVal] -> ThrowsError LispVal
strBoolBinop func op params = do
  (v1,v2) <- verifyBinop func params
  s1 <- unpackString v1
  s2 <- unpackString v2
  return $ Bool $ op s1 s2

stringOps :: [ (String, [LispVal] -> ThrowsError LispVal) ]
stringOps = let f (func,op) = (func, strBoolBinop func op)
  in map f [ ("string=?", (==))
           , ("string<=?", (<=))
           , ("string>=?", (>=))
           ]
