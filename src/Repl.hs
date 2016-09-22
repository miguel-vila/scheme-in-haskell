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
import Data.IORef
import Utils
import ReplState
import Autocompletion
import ReplCommands

evalString :: Env -> String -> IO (String, Bool)
evalString env expr =
  case readExpr expr of
    Left err ->
      return (show err, True)
    Right parsed -> do
      evalResult <- runErrorT $ eval env parsed
      return $ case evalResult of
        Left err -> (show err, True)
        Right val ->(show val, False)

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr = putStrLn =<< fst <$> evalString env expr

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action =
  do result <- prompt
     if pred result
       then return ()
       else action result >> until_ pred prompt action

readParens :: Int -> String -> ThrowsError Int
readParens openParens ""       = return openParens
readParens openParens ('(':xs) = readParens (openParens + 1) xs
readParens openParens (')':xs) =
  if openParens == 0
  then throwError_ $ Default "unbalanced parens"
  else readParens (openParens - 1) xs
readParens openParens (_:xs)   = readParens openParens xs

insertCommandExecution :: ReplState -> String -> String -> ReplState
insertCommandExecution replState input output =
  replState { inputs = (inputs replState) ++ [input]
            , outputs = (outputs replState) ++ [output]
            }

data CommandResponse = WaitForNextLine | CommandResult String

handleLispInput :: Env -> ReplState -> String -> IO (CommandResponse, ReplState)
handleLispInput env replState command =
  let currentBuffer = buffer replState
      currentOpenParens = maybe 0 parensToClose currentBuffer
      newMultilineBuffer openParens command = maybe
        (MultiLineBuffer openParens command)
        (\multilineBuffer -> MultiLineBuffer openParens ((bufferContent multilineBuffer) ++ "\n" ++ command) )
        currentBuffer
      commandToExecute currentLine = maybe currentLine (\multilineBuffer -> (bufferContent multilineBuffer) ++ "\n" ++ currentLine) currentBuffer
      evalCommand replState "" = return (WaitForNextLine, replState)
      evalCommand replState inputCommand = do
        (result, error) <- evalString env inputCommand
        let replState' = if error then resetBuffer replState else insertCommandExecution replState inputCommand result
        return (CommandResult result, replState')
  in case readParens currentOpenParens command of
       Right n | n > 0 ->
                 let buffer' = Just $ newMultilineBuffer n command
                     replState' = replState { buffer = buffer' }
                 in return (WaitForNextLine, replState')
       Right 0 ->
         evalCommand (resetBuffer replState) (commandToExecute command)
       _ ->
         evalCommand replState (commandToExecute command)

runRepl :: IO ()
runRepl = do
  let settings env = setComplete (lispCompletion env) defaultSettings
  let loop replState env =
        do input <- getInputLine $ maybe "Lisp> " (const "    | ") (buffer replState)
           case input of
             Just (':':replCommand) ->
               do (replState', output) <- liftIO $ handleReplCommand env replState replCommand
                  outputStrLn output
                  loop replState' env
             Just lispInput -> do
               (commandResult, replState') <- liftIO $ handleLispInput env replState lispInput
               case commandResult of
                 WaitForNextLine ->
                   loop replState' env
                 CommandResult result -> do
                   outputStrLn result
                   loop replState' env
             Nothing      -> return ()
  env <- primitiveBindings
  runInputT (settings env) (loop initialReplState env)

