module BooleanOps where

import LispExp
import LispError
import Unpacking
import BinaryOps

booleanBinop :: (Bool -> Bool -> Bool) -> [LispVal] -> ThrowsError LispVal
booleanBinop op params =
  Bool <$> binop unpackBool op params

booleanOps :: [ (String, [LispVal] -> ThrowsError LispVal) ]
booleanOps = [ ("&&", booleanBinop (&&))
             , ("||", booleanBinop (||))
             ]


