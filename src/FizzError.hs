{-# LANGUAGE DeriveAnyClass #-}

module FizzError
  (FizzError (..),
  ) where

import Data.Semigroup


-- FizzBuzzFib can fail with bad input three ways: 
--   if you gave it input that wasn't an integer,
--   You fed in more that one piece of data,
--   or you didn't provide any data at all
--   FibError captures this

data FizzError
  = NotAnInteger
  | NotNatural
  | OnlyOne
  | NoInput
    deriving Eq

instance Show FizzError where
  show NotAnInteger = "not an integer"
  show NotNatural   = "input needs to be zero or greater"
  show OnlyOne      = "Just pass in one number that decribes how many" ++
                      " fibonacci numbers you want for fizzbuzz."
  show NoInput      = "You need to pass in an integer that describes how" ++
                      " many fibonacci numbers you want for fizzbuzz."

