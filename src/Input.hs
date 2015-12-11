module Input
  (mustHaveOne
  ,convertToNatural
  ) where


import FizzError

import              Data.Char (isDigit)
import              Text.Read (readMaybe)
import              Data.Either.Utils (maybeToEither)

mustHaveOne :: [String] -> Either FizzError String
mustHaveOne (arg:[]) = Right arg
mustHaveOne []       = Left NoInput
mustHaveOne _        = Left OnlyOne
 

convertToNatural :: String -> Either FizzError Integer
convertToNatural str =
  isNatural =<< toDigit
  where
    toDigit = maybeToEither NotAnInteger (readMaybe str)
    isNatural int = boolToEither (int >= 0) NotNatural int

boolToEither :: Bool -> a -> b -> Either a b
boolToEither bool a b =
  case bool of
    True -> Right b
    False -> Left a
