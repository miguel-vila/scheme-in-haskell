module ReplState where

data MultiLineBuffer = MultiLineBuffer { parensToClose :: Int
                                       , bufferContent :: String
                                       }

data ReplState = ReplState { buffer :: Maybe MultiLineBuffer
                           , inputs :: [String]
                           , outputs :: [String]
                           }

resetBuffer :: ReplState -> ReplState
resetBuffer replState = replState { buffer = Nothing }

initialReplState :: ReplState
initialReplState = ReplState Nothing [] []
