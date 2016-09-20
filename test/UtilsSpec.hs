module UtilsSpec where

import Test.Hspec
import Utils

somePrimitives = [ "+"
                 , "/"
                 , "remainder"
                 , "symbol?"
                 , "<"
                 , "<="
                 , "eqv?"
                 , "string<=?"
                 , "cdr"
                 , "-"
                 , "mod"
                 , "&&"
                 , "string?"
                 , ">"
                 , "/="
                 , "equal?"
                 , "string>=?"
                 , "cons"
                 , "*"
                 , "quotient"
                 , "||"
                 , "number?"
                 , ">="
                 , "="
                 , "string=?"
                 , "car"
                 ]

utilsSpec = do
  describe "Utils" $ do
    it "matches a prefix and returns the rest" $ do
      match
        "ho"
        "hola" `shouldBe` Just ("la", "hola")
      match
        "hola"
        "hola" `shouldBe` Just ("", "hola")
      match
        "holaa"
        "hola" `shouldBe` Nothing
      match
        "abc"
        "def" `shouldBe` Nothing
      match
        "con"
        "car" `shouldBe` Nothing
      match
        "c"
        "+" `shouldBe` Nothing
      match
        "c"
        "remainder" `shouldBe` Nothing
      findMatches "c" somePrimitives
        `shouldBe` [("dr","cdr"), ("ons","cons"), ("ar","car")]
      findMatches "st" somePrimitives
        `shouldBe` [("ring<=?","string<=?"),("ring?","string?"),("ring>=?","string>=?"),("ring=?","string=?")]
