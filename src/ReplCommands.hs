module ReplCommands where

import ReplState
import ReplLoop
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

type ReplCommand = Env -> ReplState -> Loop -> InputT IO ()

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
          (tempfile, temph) <- openTempFile tempdir "session.scheme"
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
showHistory env replState loop =
  do outputStrLn $ toFileContent (inputs replState) (outputs replState)
     loop replState env

editHistory :: ReplCommand
editHistory env replState loop =
  let content = toFileContent (inputs replState) (outputs replState)
  in do finalContent <- liftIO $ fromEditor content
        outputStrLn finalContent
        loop replState env

replCommands :: [(String, ReplCommand)]
replCommands = [ ("history", showHistory)
                  , ("edit", editHistory)
                  ]

handleReplCommand :: Env -> ReplState -> Loop -> String -> InputT IO ()
handleReplCommand env replState loop command =
  case lookup command replCommands of
    Just replCommand -> replCommand env replState loop
    Nothing -> do
      outputStrLn $ "UNKNOWN REPL COMMAND : " ++ command
      loop replState env

commentOutOutput =
  intercalate "\n" . map (';':) . splitOn "\n"

toFileContent :: [String] -> [String] -> String
toFileContent inputs outputs =
  intercalate "\n" $ map (\(input,output) -> input ++ "\n" ++ (commentOutOutput output) )  $ inputs `zip` outputs

fromFileContent :: String -> [String]
fromFileContent fileContent =
  let lines = splitOn fileContent "\n"
      isComment (';':_) = True
      isComment _       = False
      getInputLines []     acc = acc
      getInputLines ls acc =
        let (inputs, rest) = break isComment ls
            acc' = acc ++ inputs
            (_,ls') = break (not . isComment) rest
        in getInputLines ls' acc'
  in getInputLines lines []
