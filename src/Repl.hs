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
import ReplLoop
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

handleCommand :: Env -> ReplState -> Loop -> String -> InputT IO ()
handleCommand env replState loop "" = loop replState env
handleCommand env replState loop inputCommand = do
  (result, error) <- liftIO $ evalString env inputCommand
  outputStrLn $ result
  let replState' = if error then resetBuffer replState else insertCommandExecution replState inputCommand result
  (loop replState' env)

insertCommandExecution :: ReplState -> String -> String -> ReplState
insertCommandExecution replState input output =
  replState { inputs = (inputs replState) ++ [input]
            , outputs = (outputs replState) ++ [output]
            }

runRepl :: IO ()
runRepl =
  let settings env = setComplete (lispCompletion env) defaultSettings
      loop :: Loop
      loop replState env =
        let currentBuffer = buffer replState
            currentOpenParens = maybe 0 parensToClose currentBuffer
            newMultilineBuffer openParens command = maybe
              (MultiLineBuffer openParens command)
              (\multilineBuffer -> MultiLineBuffer openParens ((bufferContent multilineBuffer) ++ "\n" ++ command) )
              currentBuffer
            prompt = getInputLine $ maybe "Lisp> " (const "    | ") currentBuffer
            commandToExecute currentLine = maybe currentLine (\multilineBuffer -> (bufferContent multilineBuffer) ++ "\n" ++ currentLine) currentBuffer
        in do input <- prompt
              case input of
                Just (':':replCommand) ->
                  handleReplCommand env replState loop replCommand
                Just command ->
                  case readParens currentOpenParens command of
                    Right n | n > 0 ->
                              let buffer' = Just $ newMultilineBuffer n command
                                  replState' = replState { buffer = buffer' }
                              in loop replState' env
                    Right 0 ->
                      handleCommand env (resetBuffer replState) loop (commandToExecute command)
                    _ ->
                      handleCommand env replState loop (commandToExecute command)
                Nothing      -> return ()
  in do env <- primitiveBindings
        runInputT (settings env) (loop initialReplState env)
