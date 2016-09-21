module Autocompletion where

import System.Console.Haskeline.Completion
import Data.IORef
import ReplState
import LispExp
import ReplCommands (replCommands)
import Utils

takeUntilSpecialChars :: String -> String
takeUntilSpecialChars = takeWhile (not . isSpecialChar)
  where isSpecialChar '(' = True
        isSpecialChar ')' = True
        isSpecialChar ' ' = True
        isSpecialChar '\t' = True
        isSpecialChar '\n' = True
        isSpecialChar _ = False

otherKeywords :: [String]
otherKeywords = [ "define"
                , "lambda"
                , "cond"
                , "case"
                , "if"
                , "quote"
                ]

lispCompletion :: Env -> CompletionFunc IO
lispCompletion envRef (left,_) =
  do let prefix = reverse $ takeUntilSpecialChars left
     env <- readIORef envRef
     let (lispDefinedVarsNames,_) = unzip env
     let specialCommandNames = map (':':) $ map fst $ replCommands
     let possibleNames = lispDefinedVarsNames ++ specialCommandNames ++ otherKeywords
     let completions = findCompletions prefix possibleNames
     return (left, completions)

