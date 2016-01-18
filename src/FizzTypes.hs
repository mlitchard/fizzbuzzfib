{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE DeriveDataTypeable #-}

module FizzTypes
  ( FizzError  (..)
  , FFW
  , BF
  , Fibonator  
  , Fizzanator 
  , FizzComp   (..)
  , Args       (..)
  , FibbComp   (..)
  ) where

import              Data.Semigroup
import              Control.Applicative
import              Data.Typeable
import              Data.Data
data FFW -- Fastest Fib in the West 
data BF  -- Binet's Formula
         -- https://wiki.haskell.org/The_Fibonacci_sequen

data FizzComp =
     MOD3
   | MOD5
   | PRIME
   | STANDARD 
   | PLUSPRIME
      deriving (Read,Eq,Show,Data,Typeable)

data FibbComp =
     LINEAR
   | CONSTANT
      deriving (Read,Eq,Show,Data,Typeable)

type Fibonator  = (Integer -> Integer)
type Fizzanator = (Integer -> Maybe String)
 
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
  | NoFibImp
  | NoFizzImp
    deriving Eq

instance Show FizzError where
  show NotAnInteger = "not an integer"
  show NotNatural   = "input needs to be zero or greater"
  show NotFizzy     = "You need to pick a known set of fizzbuzz computation"
  show OnlyOne      = "Just pass in one number that decribes how many" ++
                      " fibonacci numbers you want for fizzbuzz."
  show NoInput      = "You need to pass in an integer that describes how" ++
                      " many fibonacci numbers you want for fizzbuzz."
  show NoFibImp     = "You selected a Fib configration " ++ 
                      "that hasn't been implemented"
  show NoFizzImp    = "You selected a Fizz configration " ++
                      "that hasn't been implemented"

data Args = Args
  { fizz :: FizzComp
  , fib  :: FibbComp
  } deriving (Show,Data,Typeable)
