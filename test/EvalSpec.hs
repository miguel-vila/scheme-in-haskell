module EvalSpec where

import Parser
import Test.Hspec
import LispExp
import Eval
import LispError
import Control.Exception (evaluate)
import Control.Monad.Error
import Control.Monad.Error.Class

parseAndEval :: String -> IO (ThrowsError LispVal)
parseAndEval str = runErrorT $ do
  exp <- ErrorT $ return $ readExpr str
  eval exp

shouldEvaluateTo :: String -> LispVal -> Expectation
shouldEvaluateTo str expected =
  parseAndEval str `shouldReturn` return expected

shouldThrowError :: String -> LispError -> Expectation
shouldThrowError str error =
  parseAndEval str `shouldReturn` throwError error

evalSpec = describe "Eval" $ do
    it "evaluates arithmetic expressions" $ do
      "(+ 1 3)" `shouldEvaluateTo` Integer 4
      "(+ (* 4 3 2) (- 10 7))" `shouldEvaluateTo` Integer 27
    it "coerces strings to numbers" $ do
      "(+ 1 \"2\")" `shouldEvaluateTo` Integer 3
    it "handles ifs statements" $ do
      "(if (< 5 3) 6 7)" `shouldEvaluateTo` Integer 7
    it "cons lists" $ do
      "(cons 1 '())" `shouldEvaluateTo` List [Integer 1]
      "(cons 1 (cons 2 (cons 3 '())))" `shouldEvaluateTo` List [Integer 1, Integer 2, Integer 3]
    it "car lists" $ do
      "(car (cons 1 '()))" `shouldEvaluateTo` Integer 1
      "(car '())" `shouldEvaluateTo` List []
    it "cdr lists" $ do
      "(cdr (cons 1 (cons 2 (cons 3 '()))))" `shouldEvaluateTo` List [Integer 2, Integer 3]
    it "eqv?" $ do
      "(eqv? 1 2)" `shouldEvaluateTo` Bool False
      "(eqv? \"1\" \"2\")" `shouldEvaluateTo` Bool False
      "(eqv? 1 1)" `shouldEvaluateTo` Bool True
      "(eqv? \"1\" 1)" `shouldEvaluateTo` Bool False
    it "equal?" $ do
      "(equal? \"1\" 1)" `shouldEvaluateTo` Bool True
    -- @TODO:
    --it "equal?" $ do
      --"(equal? '(1 \"2\") '(1 2))" `shouldEvaluateTo` Bool True
    it "eqv?" $ do
      "(eqv? '(1 \"2\") '(1 2))" `shouldEvaluateTo` Bool False
    it "cond" $ do
      "(cond ((< 1 3) 4))" `shouldEvaluateTo` Integer 4
      "(cond ((< 4 3) 4))" `shouldEvaluateTo` List []
      "(cond ((< 1 3) 0) ((< 2 3) 1))" `shouldEvaluateTo` Integer 0
      "(cond ((< 3 2) 0) ((< 1 3) 1))" `shouldEvaluateTo` Integer 1
      "(cond ((= 1 3) 0) ((= 2 3) 1))" `shouldEvaluateTo` List []
      "(cond ((= 1 3) 0) ((= 2 3) 1) (else 4))" `shouldEvaluateTo` Integer 4
      "(cond ((= 1 3) 0) (else 4) ((= 2 3) 1))" `shouldThrowError` Default "else clause must be the last one"
    it "case" $ do
      "(case (+ 2 3) ((5 6 7) 10))" `shouldEvaluateTo` Integer 10
      "(case (* 3 7) ((5 6 7) 11) ((43 21 10) 100))" `shouldEvaluateTo` Integer 100
      "(case (+ 20 3) ((5 6 7) 10) (else 123))" `shouldEvaluateTo` Integer 123
