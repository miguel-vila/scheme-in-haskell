module Env where

import LispExp
import LispError
import Data.IORef
import Control.Monad.Error.Class
import Control.Monad.Error

nullEnv :: IO Env
nullEnv = newIORef []

isBound :: Env -> String -> IO Bool
isBound envRef var =
  do env <- readIORef envRef
     let searched = lookup var env
     return $ maybe False (const True) searched

getVar :: Env -> String -> IOThrowsError LispVal
getVar envRef name = ErrorT $
  do env <- readIORef envRef
     let searched = lookup name env
     case searched of
       Just value ->
         return <$> readIORef value
       Nothing    ->
         return $ throwError $ UnboundVar "Getting an unbound variable" name

setVar :: Env -> String -> LispVal -> IOThrowsError ()
setVar envRef name value = ErrorT $
  do env <- readIORef envRef
     let searched = lookup name env
     case searched of
       Just valueRef -> return <$> (writeIORef valueRef value)
       Nothing       -> return  $  throwError $ UnboundVar "Getting an unbound variable" name

defineVar :: Env -> String -> LispVal -> IOThrowsError ()
defineVar envRef name value = ErrorT $
  do alreadyDefined <- isBound envRef name
     if alreadyDefined
       then runErrorT $ setVar envRef name value
       else do valueRef <- newIORef value
               env <- readIORef envRef
               return <$> writeIORef envRef ((name,valueRef):env)

bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars envRef vars =
  let varBinding (name, val) = do
        ref <- newIORef val
        return (name,ref)
      extendEnvWith env = do
        newBindings <- mapM varBinding vars
        return (newBindings ++ env)
  in do env <- readIORef envRef
        extendedEnv <- extendEnvWith env
        newIORef extendedEnv
