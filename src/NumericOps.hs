module NumericOps where

import LispExp
import LispError
import BinaryOps
import Unpacking

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op params =
  Integer <$> binop unpackNum op params

numericOps :: [ (String, [LispVal] -> ThrowsError LispVal) ]
numericOps = [ ("+", numericBinop (+))
             , ("-", numericBinop (-))
             , ("*", numericBinop (*))
             , ("/", numericBinop div)
             , ("mod", numericBinop mod)
             , ("quotient", numericBinop quot)
             , ("remainder", numericBinop rem)
             ]


