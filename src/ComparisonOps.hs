{-# LANGUAGE ExistentialQuantification #-}

module ComparisonOps where

import Unpacking
import LispExp
import LispError
import Control.Monad.Error.Class (catchError)

isTrue :: LispVal -> Bool
isTrue (Bool True) = True
isTrue _           = False

allTrue :: [LispVal] -> LispVal
allTrue xs = Bool $ all isTrue xs

comparisonOp :: (Integer -> Integer -> Bool) -> [LispVal] -> ThrowsError LispVal
comparisonOp op params = do
  bools <- mapM unpackNum params
  return $ Bool $ all id $ zipWith op bools (tail bools)

eqv :: [LispVal] -> ThrowsError LispVal
eqv = eqs eqv

eqs :: ([LispVal] -> ThrowsError LispVal) -> [LispVal] -> ThrowsError LispVal
eqs _ [Atom str1, Atom str2] = return $ Bool (str1 == str2)
eqs compare [List xs1, List xs2] =
  if length xs1 == length xs2
  then do
    eqvs <- mapM (eqs compare) $ zipWith (\x1 x2 -> [x1, x2]) xs1 xs2
    return $ allTrue eqvs
  else return $ Bool False
eqs compare [DottedList xs1 tl1, DottedList xs2 tl2] =
  if length xs1 == length xs2
  then do
    eqvs <- mapM (eqs compare) $ zipWith (\x1 x2 -> [x1, x2]) xs1 xs2
    let allEqvs = allTrue eqvs
    eqvtl <- eqs compare [tl1, tl2]
    return $ allTrue [allEqvs, eqvtl]
  else return $ Bool False
eqs _ [Integer n1, Integer n2] = return $ Bool $ n1 == n2
eqs _ [Float n1, Float n2] = return $ Bool $ n1 == n2
eqs _ [String n1, String n2] = return $ Bool $ n1 == n2
eqs _ [Bool n1, Bool n2] = return $ Bool $ n1 == n2
eqs _ [Character n1, Character n2] = return $ Bool $ n1 == n2
eqs _ [_,_] = return $ Bool False
eqs _ badArgList = throwError_ $ NumArgs 2 badArgList

data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> ThrowsError a)

equalsUnderUnpacker :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
equalsUnderUnpacker arg1 arg2 (AnyUnpacker unpack) =
  do unpacked1 <- unpack arg1
     unpacked2 <- unpack arg2
     return $ unpacked1 == unpacked2
  `catchError` (const $ return False)

unpackers :: [ Unpacker ]
unpackers = [ AnyUnpacker unpackNum
            , AnyUnpacker unpackBool
            , AnyUnpacker unpackString
            ]

equal :: [LispVal] -> ThrowsError LispVal
equal [arg1, arg2] =
  do eq1         <- or <$> mapM (equalsUnderUnpacker arg1 arg2) unpackers
     Bool( eq2 ) <- eqs equal [arg1, arg2]
     return $ Bool ( eq1 || eq2 )


comparisons :: [ (String, [LispVal] -> ThrowsError LispVal) ]
comparisons = [ ("<", comparisonOp (<))
              , (">", comparisonOp (>))
              , (">=", comparisonOp (>=))
              , ("<=", comparisonOp (<=))
              , ("/=", comparisonOp (/=))
              , ("=", comparisonOp (==))
              , ("eqv?", eqv)
              , ("equal?", equal)
              ]

