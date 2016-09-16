import Parser
import LispExp
import Eval
import LispError
import Test.Hspec
import Control.Exception (evaluate)
import Control.Monad.Error
import Control.Monad.Error.Class
import ParsingSpec
import EvalSpec

main :: IO ()
main = hspec $ do
  parsingSpec
  evalSpec
