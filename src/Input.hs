module Input
  (mustHaveOne
  ,convertToDigit
  ) where


import FizzError

import              Data.Char (isDigit)
import              Text.Read (readMaybe)
import              Data.Either.Utils (maybeToEither)

mustHaveOne :: [String] -> Either FizzError String
mustHaveOne (arg:[]) = Right arg
mustHaveOne []       = Left NoInput
mustHaveOne _        = Left OnlyOne
 

convertToDigit :: String -> Either FizzError Integer
convertToDigit str =
  maybeToEither NotAnInteger (readMaybe str)    
