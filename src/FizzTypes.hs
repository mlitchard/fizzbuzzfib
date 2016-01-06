{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE Rank2Types #-}

module FizzTypes
  ( FizzError (..)
  , FFW
  , BF
  , Fibonator (..)
  , Fizzanator (..)
  , FizzComp (..)
  ) where

import              Data.Semigroup
import              Control.Applicative
data FFW -- Fastest Fib in the West 
data BF  -- Binet's Formula
         -- https://wiki.haskell.org/The_Fibonacci_sequen

data FizzComp =
     MOD3
   | MOD5
   | PRIME
   | STANDARD 
   | PLUSPRIME
      deriving (Read,Eq,Show)

data Options = Options
  { fib_function :: Fibonator t
  , fizz_comp    :: Fizzanator
  }
newtype Fibonator t = Fibonator (Integer -> Integer)
newtype Fizzanator  = Fizzanator (Integer -> Maybe String)
-- FizzBuzzFib can fail with bad input three ways: 
--   if you gave it input that wasn't an integer,
--   You fed in more that one piece of data,
--   or you didn't provide any data at all
--   FibError captures this

data FizzError
  = NotAnInteger
  | NotNatural
  | NotFizzy
  | OnlyOne
  | NoInput
    deriving Eq

instance Show FizzError where
  show NotAnInteger = "not an integer"
  show NotNatural   = "input needs to be zero or greater"
  show NotFizzy     = "You need to pick a known set of fizzbuzz computation"
  show OnlyOne      = "Just pass in one number that decribes how many" ++
                      " fibonacci numbers you want for fizzbuzz."
  show NoInput      = "You need to pass in an integer that describes how" ++
                      " many fibonacci numbers you want for fizzbuzz."

