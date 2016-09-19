module Main where

import System.Environment (getArgs)
import Repl
import Env

main :: IO ()
main = do
  args <- getArgs
  env <- nullEnv
  case length args of
    0 -> runRepl
    1 -> evalAndPrint env (head args)
    _ -> putStrLn " Program takes only 0 or 1 argument"
