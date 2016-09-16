module Conditionals where

import LispExp
import LispError
import BooleanOps
import ComparisonOps
import Control.Monad.Error.Class

type Eval = LispVal -> IOThrowsError LispVal

lispIf :: Eval -> LispVal -> LispVal -> LispVal -> IOThrowsError LispVal
lispIf eval cond conseq altern =
  do
  result <- eval cond
  case result of
    Bool False -> eval altern
    _          -> eval conseq

lispCond :: Eval -> [LispVal] -> IOThrowsError LispVal
lispCond eval clauses = do
   result <- findCondMatch eval clauses
   case result of
     Just match -> eval match
     Nothing    -> return $ List []

lispCase :: Eval -> LispVal -> [LispVal] -> IOThrowsError LispVal
lispCase eval value clauses =
  do v <- eval value
     case_result <- findCaseMatch v clauses
     case case_result of
       Just match -> eval match
       Nothing    -> return $ List []

findCondMatch :: Eval -> [LispVal] -> IOThrowsError (Maybe LispVal)
findCondMatch _ [] = return Nothing
findCondMatch _ (List [Atom "else", expr]:[]) = return $ Just expr
findCondMatch _ (List [Atom "else",_]:_) = throwError $ Default "else clause must be the last one"
findCondMatch eval (List [condition, expr]:rest) =
  do result <- eval condition
     case result of
       Bool False -> findCondMatch eval rest
       _          -> return $ Just expr
findCondMatch _ (badForm:_) =
  throwError $ BadSpecialForm "Expected two elements for each cond clause" badForm

findCaseMatch :: LispVal -> [LispVal] -> IOThrowsError (Maybe LispVal)
findCaseMatch _ []        = return Nothing
findCaseMatch _ (List [Atom "else", expr]:[]) = return $ Just expr
findCaseMatch _ (List [Atom "else", expr]:_) = throwError $ Default "else clause must be the last one"
findCaseMatch value ((List [List caseValues, exp]):rest) =
  case mapM (\x -> eqv [value, x]) caseValues of
    Left err -> throwError err
    Right matches ->
      if any isTrue matches
      then return $ return exp
      else findCaseMatch value rest
--findCaseMatch _ ((List badForm):_) =
--  return $ throwError_ $ BadSpecialForm "Expected a list with two elements for each case" badForm
findCaseMatch _ (badForm:_) =
  throwError $ BadSpecialForm "Expected a list of cases" badForm
