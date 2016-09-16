module Eval where

import LispExp
import LispError
import System.Environment
import Control.Monad
import Control.Monad.Error.Class (catchError, throwError)
import LispList
import StringOps
import NumericOps
import ComparisonOps
import BooleanOps
import Conditionals
import TypeQueries
import Control.Monad.Error 

eval :: LispVal -> IOThrowsError LispVal
eval val@(String _) = return val
eval val@(Integer _) = return val
eval val@(Float _) = return val
eval val@(Bool _) = return val
eval (List [Atom "quote", val]) = return val
eval (List [Atom "if", cond, conseq, altern]) = lispIf eval cond conseq altern
eval (List (Atom "cond" : clauses)) = lispCond eval clauses
eval (List [Atom "cond", badClauses]) = throwError $ TypeMismatch "list" badClauses
eval (List (Atom "case" : value : clauses)) = lispCase eval value clauses
eval (List (Atom func : args)) = apply func =<< mapM eval args
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

primitives :: [ (String, [LispVal] -> ThrowsError LispVal) ]
primitives = numericOps
             ++ booleanOps
             ++ typeQueries
             ++ comparisons
             ++ stringOps
             ++ listOps

apply :: String -> [LispVal] -> IOThrowsError LispVal
apply func args = case lookup func primitives of
  Just f  -> ErrorT $ return $ f args
  Nothing -> throwError $ NotFunction "Unrecognized primitive function args" func

