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

evalString :: Env -> String -> IO (String, Bool)
evalString env expr =
  case readExpr expr of
    Left err ->
      do putStrLn $"PARSE ERROR: " ++ (show expr)
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

readParens :: Int -> String -> ThrowsError Int
readParens openParens ""       = return openParens
readParens openParens ('(':xs) = readParens (openParens + 1) xs
readParens openParens (')':xs) =
  if openParens == 0
  then throwError_ $ Default "unbalanced parens"
  else readParens (openParens - 1) xs
readParens openParens (_:xs)   = readParens openParens xs

type Loop = ReplState -> Env -> InputT IO ()

handleCommand :: Env -> ReplState -> Loop -> String -> InputT IO ()
handleCommand env replState loop "" = loop replState env
handleCommand env replState loop inputCommand = do
  (result, error) <- liftIO $ evalString env inputCommand
  outputStrLn $ result
  let replState' = if error then resetReplState replState else replState
  (loop replState' env)

resetReplState :: ReplState -> ReplState
resetReplState replState = replState { buffer = Nothing }

runRepl :: IO ()
runRepl = let settings env = setComplete (lispCompletion env) defaultSettings
              loop :: Loop
              loop replState env =
                let currentBuffer = buffer replState
                    currentOpenParens = maybe 0 parensToClose currentBuffer
                    newMultilineBuffer openParens command = maybe
                      (MultiLineBuffer openParens command)
                      (\multilineBuffer -> MultiLineBuffer openParens ((bufferContent multilineBuffer) ++ " " ++ command) )
                      currentBuffer
                    prompt = getInputLine $ maybe "Lisp> " (const "    | ") currentBuffer
                    commandToExecute currentLine = maybe currentLine (\multilineBuffer -> (bufferContent multilineBuffer) ++ " " ++ currentLine) currentBuffer
                in do input <- prompt
                      case input of
                        Just command ->
                          case readParens currentOpenParens command of
                            Right n | n > 0 ->
                                      let buffer' = Just $ newMultilineBuffer n command
                                          replState' = replState { buffer = buffer' }
                                      in loop replState' env
                            Right 0 ->
                              handleCommand env (resetReplState replState) loop (commandToExecute command)
                            _ ->
                              handleCommand env replState loop (commandToExecute command)
                        Nothing      -> return ()
          in do env <- primitiveBindings
                runInputT (settings env) (loop initialReplState env)

data MultiLineBuffer = MultiLineBuffer { parensToClose :: Int
                                       , bufferContent :: String
                                       }

data ReplState = ReplState { buffer :: Maybe MultiLineBuffer }

initialReplState :: ReplState
initialReplState = ReplState $ Nothing
