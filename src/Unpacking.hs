module Unpacking where

import LispExp
import LispError
import Numeric (readDec)

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Integer n) = return n
unpackNum (String str) = case readDec str of
  [(n,"")] -> return n
  _ -> throwError_ $ TypeMismatch "number" (String str)
unpackNum badExp = throwError_ $ TypeMismatch "number" badExp

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool x) = return x
unpackBool badExp = throwError_ $ TypeMismatch "boolean" badExp

unpackString :: LispVal -> ThrowsError String
unpackString (String s) = return s
unpackString badExp = throwError_ $ TypeMismatch "string" badExp

unpackParameter :: LispVal -> ThrowsError String
unpackParameter (Atom param) = return param
unpackParameter badExp = throwError_ $ TypeMismatch "atom" badExp
