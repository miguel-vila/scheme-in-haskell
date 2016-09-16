module BinaryOps where

import LispError
import LispExp

binop :: (LispVal -> ThrowsError a) -> (a -> a -> a) -> [LispVal] -> ThrowsError a
binop unpacker op params = foldl1 op <$> mapM unpacker params


verifyBinop :: String -> [LispVal] -> ThrowsError (LispVal, LispVal)
verifyBinop _ [ v1 , v2 ] = return (v1,v2)
verifyBinop func args = throwError_ $ NumArgs 2 args

