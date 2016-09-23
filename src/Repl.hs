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

handleLispInput :: Env -> ReplState -> String -> InputT IO (ReplState, Maybe String)
handleLispInput env replState command =
  let currentBuffer = buffer replState
      currentOpenParens = maybe 0 parensToClose currentBuffer
      newMultilineBuffer openParens command = maybe
        (MultiLineBuffer openParens command)
        (\multilineBuffer -> MultiLineBuffer openParens ((bufferContent multilineBuffer) ++ "\n" ++ command) )
        currentBuffer
      commandToExecute currentLine = maybe currentLine (\multilineBuffer -> (bufferContent multilineBuffer) ++ "\n" ++ currentLine) currentBuffer
      evalCommand replState "" = return (replState, Nothing)
      evalCommand replState inputCommand = do
        (result, error) <- liftIO $ evalString env inputCommand
        let replState' = if error then resetBuffer replState else insertCommandExecution replState inputCommand result
        return (replState', Just result)
  in case readParens currentOpenParens command of
       Right n | n > 0 ->
                 let buffer' = Just $ newMultilineBuffer n command
                     replState' = replState { buffer = buffer' }
                 in return (replState', Nothing)
       Right 0 ->
         evalCommand (resetBuffer replState) (commandToExecute command)
       _ ->
         evalCommand replState (commandToExecute command)

promptText :: ReplState -> String
promptText replState = maybe "Lisp> " (const "    | ") (buffer replState)

handleLispInputs :: Env -> ReplState -> [String] -> InputT IO ReplState
handleLispInputs env replState [] = return replState
handleLispInputs env replState (input:rest) = do
  outputStrLn $ (promptText replState) ++ input
  (replState', outputMaybe) <- handleLispInput env replState input
  maybe (return ()) outputStrLn outputMaybe
  handleLispInputs env replState' rest

executeLispInputs :: Env -> ReplState -> [String] -> InputT IO ReplState
executeLispInputs env replState []           = return replState
executeLispInputs env replState (input:rest) = do
  (replState', _) <- handleLispInput env replState input
  executeLispInputs env replState' rest

undo :: ReplState -> InputT IO (Env, ReplState)
undo replState =
  let newInputs = init $ inputs replState
      replState' = initialReplState
  in do env' <- liftIO primitiveBindings
        replState'' <- executeLispInputs env' replState' newInputs
        return (env', replState'')

runRepl :: IO ()
runRepl = do
  let settings env = setComplete (lispCompletion env) defaultSettings
  let loop replState env =
        do input <- getInputLine $ promptText replState
           case input of
             Just ":edit" ->
               let content = toFileContent (inputs replState) (outputs replState)
               in do finalContent <- liftIO $ fromEditor content
                     let inputs = fromFileContent finalContent
                     env' <- liftIO primitiveBindings
                     outputStrLn "** RERUNNING COMMANDS **"
                     replState' <- handleLispInputs env' initialReplState inputs
                     loop replState' env'
             Just ":undo" ->
               do (env', replState') <- undo replState
                  loop replState' env'
             Just (':':replCommand) ->
               do (replState', output) <- liftIO $ handleReplCommand env replState replCommand
                  outputStrLn output
                  loop replState' env
             Just lispInput -> do
               (replState', outputMaybe) <- handleLispInput env replState lispInput
               maybe (return ()) outputStrLn outputMaybe
               loop replState' env
             Nothing      -> return ()
  env <- primitiveBindings
  runInputT (settings env) (loop initialReplState env)

