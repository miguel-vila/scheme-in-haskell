module Repl where

import Control.Concurrent
import LispExp
import LispError
import Parser
import Eval
import System.IO
import Control.Monad.Error.Class
import Control.Monad.Error (runErrorT)
import Env
import System.Console.Haskeline
import Control.Monad.IO.Class
import System.Console.Haskeline.Completion
import Data.IORef
import Data.List (isPrefixOf)
import Utils

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt str = flushStr str >> getLine

evalString :: Env -> String -> IO String
evalString env expr =
  case readExpr expr of
    Left err -> return $ show err
    Right parsed -> do
      evalResult <- runErrorT $ eval env parsed
      return $ case evalResult of
        Left err -> show err
        Right val -> show val

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr = putStrLn =<< evalString env expr

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action =
  do result <- prompt
     if pred result
       then return ()
       else action result >> until_ pred prompt action

takeUntilSpecialChars :: String -> String
takeUntilSpecialChars = takeWhile (not . isSpecialChar)
  where isSpecialChar '(' = True
        isSpecialChar ')' = True
        isSpecialChar ' ' = True
        isSpecialChar '\t' = True
        isSpecialChar '\n' = True
        isSpecialChar _ = False

lispCompletion :: Env -> CompletionFunc IO
lispCompletion envRef (left,_) =
  do let prefix = reverse $ takeUntilSpecialChars left
     env <- readIORef envRef
     let (names,_) = unzip env
     let completions = findCompletions prefix names
     return (left, completions)

runRepl :: IO ()
runRepl = let settings env = setComplete (lispCompletion env) defaultSettings
              loop :: Env -> InputT IO ()
              loop env = do input <- getInputLine "Lisp> "
                            case input of
                              Just "" -> do
                                loop env
                              Just inputCommand -> do
                                result <- liftIO $ evalString env inputCommand
                                outputStrLn $ result
                                (loop env)
                              Nothing -> return ()
          in do env <- primitiveBindings
                runInputT (settings env) (loop env)

oldRepl :: IO ()
oldRepl = do env <- primitiveBindings
             until_ (== "quit") (readPrompt "Lisp> ") (evalAndPrint env)
