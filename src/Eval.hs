module Eval where

import System.IO.Unsafe
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
import Env
import Unpacking

readParams :: [LispVal] -> IOThrowsError [String]
readParams pars = wrap $ mapM unpackParameter pars

readBindings :: [LispVal] -> IOThrowsError [(String, LispVal)]
readBindings [] = return []
readBindings (List [Atom name, exp]:rest) =
  do others <- readBindings rest
     return $ (name, exp):others
readBindings badForm = throwError $ BadSpecialForm "Expected bindings, got:" (List badForm)

eval :: Env -> LispVal -> IOThrowsError LispVal
eval env val@(String _) = return val
eval env val@(Integer _) = return val
eval env val@(Float _) = return val
eval env val@(Bool _) = return val
eval env (List [Atom "quote", val]) = return val
eval env (List [Atom "if", cond, conseq, altern]) = lispIf (eval env) cond conseq altern
eval env (List (Atom "cond" : clauses)) = lispCond (eval env) clauses
eval env (List [Atom "cond", badClauses]) = throwError $ TypeMismatch "list" badClauses
eval env (List (Atom "case" : value : clauses)) = lispCase (eval env) value clauses
eval env (List [Atom "let", List bindingsList, body]) =
  do bindings <- readBindings bindingsList
     let (names, exps) = unzip bindings
     newVars <- zip names <$> mapM (eval env) exps
     let bindVars_ env vars = ErrorT $ return <$> bindVars env vars
     bodyEnv <- bindVars_ env newVars
     eval bodyEnv body
eval env (List (Atom "define": List (Atom var: params): body)) =
  do paramsNames <- readParams params
     func <- makeNormalFunc paramsNames body env
     defineVar env var func
     return $ List []
eval env (List (Atom "define": DottedList (Atom var: params) vararg: body)) =
  do paramsNames <- readParams params
     varar <- wrap $ unpackString vararg
     func <- makeVarArgFunc varar paramsNames body env
     defineVar env var func
     return $ List []
eval env (List [Atom "lambda", (List params), body]) =
  do paramsNames <- readParams params
     makeNormalFunc paramsNames [body] env
eval env (List [Atom "lambda", param, body]) =
  do paramName <- wrap $ unpackParameter param
     makeNormalFunc [paramName] [body] env
eval env (List [Atom "lambda", (DottedList params vararg), (List body)]) =
  do paramsNames <- readParams params
     varar <- wrap $ unpackString vararg
     makeVarArgFunc varar paramsNames body env
eval env (List [Atom "define", Atom var, form]) =
  do value <- eval env form
     defineVar env var value
     return value
eval env (List [Atom "set!", Atom var, form]) =
  do value <- eval env form
     setVar env var value
     return value
eval env (List (function : args)) = do
  do func <- eval env function
     evaluatedArgs <- mapM (eval env) args
     apply func evaluatedArgs
eval env (Atom name) =
  getVar env name
eval _ badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

primitives :: [ (String, [LispVal] -> ThrowsError LispVal) ]
primitives = numericOps
             ++ booleanOps
             ++ typeQueries
             ++ comparisons
             ++ stringOps
             ++ listOps

debug :: Show a => String -> a -> a
debug str x =
  unsafePerformIO $
  do putStrLn $ "DEBUG: " ++ str ++ ":" ++ show x
     return x

apply :: LispVal -> [LispVal] -> IOThrowsError LispVal
apply (PrimitiveFunc primitiveFunc) args =
  wrap $ primitiveFunc args
apply (Func params vararg body closure) args =
  let remainingArgs = drop (length params) args
      num = toInteger . length
      evalBody :: Env -> IOThrowsError LispVal
      evalBody env = last <$> mapM (eval env) body
      bindVarArgs env = case vararg of
        Just argName -> bindVars env [(argName, List remainingArgs)]
        Nothing -> return env
      paramsEnv :: IO Env
      paramsEnv = bindVars closure $ zip params args
      functionEnv :: IOThrowsError Env
      functionEnv = ErrorT $ fmap return $ bindVarArgs =<< paramsEnv
  in if num params /= num args && vararg == Nothing
     then wrap $ throwError $ NumArgs (num params) args
     else evalBody =<< functionEnv
apply notFunction _ = throwError $ NotFunction "Not a function" (show notFunction)

primitiveBindings :: IO Env
primitiveBindings =
  let makePrimitiveFunc (name, func) = (name, PrimitiveFunc func)
  in do env <- nullEnv
        let primitivesFuncs = map makePrimitiveFunc primitives
        bindVars env primitivesFuncs

makeFunc :: Maybe String -> [String] -> [LispVal] -> Env -> IOThrowsError LispVal
makeFunc vararg params body closure = return $ Func params vararg body closure

makeNormalFunc = makeFunc Nothing
makeVarArgFunc = makeFunc . Just
