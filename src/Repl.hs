module Repl where

import LispExp
import LispError
import Parser
import Eval
import System.IO
import Control.Monad.Error.Class
import Control.Monad.Error (runErrorT)

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt str = flushStr str >> getLine

evalString :: String -> IO String
evalString expr =
  case readExpr expr of
    Left err -> return $ show err
    Right parsed -> do
      evalResult <- runErrorT $ eval parsed
      return $ case evalResult of
        Left err -> show err
        Right val -> show val

evalAndPrint :: String -> IO ()
evalAndPrint expr = putStrLn =<< evalString expr

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m()) -> m ()
until_ pred prompt action =
  do result <- prompt
     if pred result
       then return ()
       else action result >> until_ pred prompt action

runRepl :: IO ()
runRepl =
  until_ (== "quit") (readPrompt "Lisp> ") evalAndPrint
