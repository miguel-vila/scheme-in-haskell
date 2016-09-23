module ReplCommands where

import ReplState
import LispExp
import Data.Maybe
import Data.List.Split (splitOn)
import Data.List (isPrefixOf, intercalate)
import System.Directory(getTemporaryDirectory, removeFile)
import System.IO (Handle, hFlush, stdout, hPutStr, hGetChar, hClose, openTempFile, hReady, hGetContents)
import System.Process (createProcess, waitForProcess, StdStream(CreatePipe), std_out, std_in, std_err, proc, shell)
import System.Environment(lookupEnv)
import System.Console.Haskeline
import Control.Monad.IO.Class

type ReplCommand = Env -> ReplState -> IO (ReplState, String)

fromEditor :: String -> IO String
fromEditor buffer = do
  tempfile <- dumpSessionIntoTempFile
  p_handle <- openEditor tempfile
  waitForProcess p_handle
  editedBuffer <- readFile tempfile
  removeFile tempfile
  return editedBuffer
  where dumpSessionIntoTempFile = do
          tempdir <- getTemporaryDirectory
          (tempfile, temph) <- openTempFile tempdir "session.scm"
          hPutStr temph buffer
          hFlush temph
          hClose temph
          return tempfile
        openEditor tempfile = do
          visual <- lookupEnv "VISUAL"
          editor <- lookupEnv "EDITOR"
          (_, _, _, p_handle) <- createProcess (proc (fromMaybe (fromMaybe "vim" editor) visual) [tempfile])
          return p_handle

showHistory :: ReplCommand
showHistory env replState =
  return (replState, toFileContent (inputs replState) (outputs replState))

editHistory :: ReplCommand
editHistory env replState =
  let content = toFileContent (inputs replState) (outputs replState)
  in do finalContent <- fromEditor content
        return (replState, finalContent)

replCommands :: [(String, ReplCommand)]
replCommands = [ ("history", showHistory)
               , ("edit", editHistory)
               ]

handleReplCommand :: Env -> ReplState -> String -> IO (ReplState, String)
handleReplCommand env replState command =
  case lookup command replCommands of
    Just replCommand -> replCommand env replState
    Nothing -> do
      return (replState, "UNKNOWN REPL COMMAND : " ++ command)

commentOutOutput :: String -> String
commentOutOutput =
  intercalate "\n" . map (';':) . splitOn "\n"

toFileContent :: [String] -> [String] -> String
toFileContent inputs outputs =
  intercalate "\n" $ map (\(input,output) -> input ++ "\n" ++ (commentOutOutput output) )  $ inputs `zip` outputs

fromFileContent :: String -> [String]
fromFileContent fileContent =
  let lines = splitOn "\n" fileContent
      isComment (';':_) = True
      isComment _       = False
  in filter (not . isComment) lines
