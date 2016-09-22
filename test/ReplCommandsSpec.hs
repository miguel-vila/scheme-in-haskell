module ReplCommandsSpec where

import Test.Hspec
import Test.HUnit
import ReplCommands

replCommandsSpec = describe "ReplCommands" $ do
  it "filters and separates the inputs from a file's content" $ do
    fromFileContent "(+ 3 4 5)\n\
                    \;12\n\
                    \(def (abs x)\n\
                    \(if (< x 0)\n\
                    \(- x)\n\
                    \x))\n\
                    \; ()\n\
                    \; other comment!\n\
                    \(abs (- 3))\n\
                    \;3" `shouldBe` [ "(+ 3 4 5)"
                                    , "(def (abs x)"
                                    , "(if (< x 0)"
                                    , "(- x)"
                                    , "x))"
                                    , "(abs (- 3))"
                                    ]
    fromFileContent "(def (abs x)\n\
                    \(if (< x 0)\n\
                    \(- x)\n\
                    \x))\n\
                    \; ()\n\
                    \; other comment!\n\
                    \(abs (- 3))" `shouldBe` [ "(def (abs x)"
                                             , "(if (< x 0)"
                                             , "(- x)"
                                             , "x))"
                                             , "(abs (- 3))"
                                             ]
