module LispList where

import LispError
import LispExp

car :: [LispVal] -> ThrowsError LispVal
car [List (x:_)] = return x
car [List _] = return $ List []
car [DottedList (x:_) _] = return x
car [DottedList _ _] = return $ List []
car [exp] = throwError_ $ TypeMismatch "pair" exp
car exp = throwError_ $ NumArgs 1 exp

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (_:xs)] = return $ List xs
cdr [List _] = return $ List []
cdr [DottedList (_:xs) tl] = return $ DottedList xs tl
cdr [DottedList xs tl] = return tl
cdr [exp] = throwError_ $ TypeMismatch "pair" exp
cdr exp = throwError_ $ NumArgs 1 exp

cons :: [LispVal] -> ThrowsError LispVal
cons [x, (List [])] = return $ List [x]
cons [x, (List xs)] = return $ List (x:xs)
cons [_, badExp]    = throwError_ $ TypeMismatch "lisp" badExp
cons badExp         = throwError_ $ NumArgs 2 badExp

listOps :: [ (String, [LispVal] -> ThrowsError LispVal) ]
listOps = [ ("car", car)
          , ("cdr", cdr)
          , ("cons", cons)
          ]
