module Utils where

import Data.Maybe
import System.Console.Haskeline.Completion

match :: String -> String -> Maybe (String,String)
match a b = do suffix <- loop a b
               return (suffix, b)
  where loop (x:xs) (y:ys) =
          if x == y
          then loop xs ys
          else Nothing
        loop [] [] = Just []
        loop _  [] = Nothing
        loop [] (y:ys) = Just (y:ys)

findMatches :: String -> [String] -> [(String,String)]
findMatches prefix xs =
  mapMaybe (match prefix) xs

-- match "co" "cons" == Maybe ("ns","cons")
-- match "co" "cdr" ==  Nothing

findCompletions :: String -> [String] -> [Completion]
findCompletions prefix words =
  do (suffix, full) <- findMatches prefix words
     return $ Completion suffix full (suffix == "")
