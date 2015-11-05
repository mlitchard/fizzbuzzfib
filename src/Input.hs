module Input
  (mustHaveOne
  ,convertToDigit
  ) where


import FizzError

import Data.Char (isDigit)
import Text.Read (read)


mustHaveOne :: [String] -> Either FizzError String
mustHaveOne (arg:[]) = Right arg
mustHaveOne []       = Left NoInput
mustHaveOne _        = Left OnlyOne
 

convertToDigit :: String -> Either FizzError Integer
convertToDigit str =
  let 
      test    = map isDigit str
      isBogus = elem False test
  in case isBogus of
       True  -> Left NotAnInteger
       False -> Right (read str :: Integer)
