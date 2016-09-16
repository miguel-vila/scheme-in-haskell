module ParsingSpec where

import Parser
import Test.Hspec
import LispExp

shouldBeParsedTo :: String -> LispVal -> Expectation
shouldBeParsedTo str expected =
  readExpr str `shouldBe` return expected
  
parsingSpec = do
  describe "Parsing" $ do
    it "parses an integer" $ do
      "123" `shouldBeParsedTo` Integer 123
    it "parses a float" $ do
      "12.345" `shouldBeParsedTo` Float 12.345
    it "parses an arithmetic expression" $ do
      "(+ (* 5 6) (/ 2 3))" `shouldBeParsedTo`
        List
         [ Atom "+"
         , List
           [ Atom "*"
           , Integer 5
           , Integer 6
           ]
         , List
           [ Atom "/"
           , Integer 2
           , Integer 3
           ]
         ]
    it "parses boolean values" $ do
      "#t" `shouldBeParsedTo` Bool True
      "#f" `shouldBeParsedTo` Bool False
    it "parses strings" $ do
      "\"hola\"" `shouldBeParsedTo` String "hola"
    it "parses strings with quotes inside" $ do
      "\"hola \\\"hello\\\"\"" `shouldBeParsedTo` String "hola \"hello\""
    it "parse quotes" $ do
      "'(1 \"2\")" `shouldBeParsedTo` List [Atom "quote", List [ Integer 1, String "2" ]]
