module TypeQueries where

import LispExp
import LispError

isSymbol :: [LispVal] -> ThrowsError LispVal
isSymbol ((Atom _) : _) = return $ Bool True
isSymbol _              = return $ Bool False

isString :: [LispVal] -> ThrowsError LispVal
isString ((String _) : _) = return $ Bool True
isString _                = return $ Bool False

isNumber :: [LispVal] -> ThrowsError LispVal
isNumber ((Integer _) : _) = return $ Bool True
isNumber ((Float _) : _) = return $ Bool True
isNumber _ = return $ Bool False

typeQueries :: [ (String, [LispVal] -> ThrowsError LispVal) ]
typeQueries = [ ("symbol?", isSymbol)
              , ("string?", isString)
              , ("number?", isNumber)
              ]
