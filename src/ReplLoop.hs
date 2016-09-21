module ReplLoop where

import ReplState
import LispExp
import System.Console.Haskeline

type Loop = ReplState -> Env -> InputT IO ()
