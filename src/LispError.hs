module LispError where

--import Control.Monad.Error.Class
import Text.ParserCombinators.Parsec.Error
import LispExp
import Control.Monad.Error

instance Show LispError where
  show (UnboundVar message varname) = message ++ ": " ++ varname
  show (BadSpecialForm message form) = message ++ ": " ++ show form
  show (NotFunction message func) = message ++ ": " ++ func
  show (NumArgs expected found) = "Expected " ++ show expected ++ " args: found values " ++ unwordsList found
  show (TypeMismatch expected found) = "Invalid type: expected "++ expected ++ ", found " ++ show found
  show (Parser parseErr) = "Parse error at " ++ show parseErr
  show (Default err) = "Error: " ++ err

instance Error LispError where
  noMsg = Default "An error has occurred"
  strMsg = Default

trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right a) = a

throwError_ :: LispError -> ThrowsError a
throwError_ err = Left err

type IOThrowsError = ErrorT LispError IO

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err ) = throwError err
liftThrows (Right val) = return val

wrap :: ThrowsError a -> IOThrowsError a
wrap x = ErrorT $ return x
