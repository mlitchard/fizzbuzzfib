module Input
  (onlyOne
  ,convertToDigit
  ) where


import FibError

import Data.Char (isDigit)
import Text.Read (read)


onlyOne :: [String] -> Either FibError String
onlyOne (arg:[]) = Right arg
onlyOne _        = Left OnlyOne
 

convertToDigit :: String -> Either FibError Integer
convertToDigit str =
  let 
      test    = map isDigit str
      isBogus = elem False test
  in case isBogus of
       True  -> Left NotAnInteger
       False -> Right (read str :: Integer)
