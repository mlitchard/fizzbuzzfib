{-# LANGUAGE MonadComprehensions #-}
module FizzBuzz
    (fizzbuzz
    ,fib 
    ,fizzBuzzFib
    ) where

import FizzError
import Input
import Data.Semigroup ((<>),getOption)
import Data.Maybe (fromMaybe)
import Data.Numbers.Primes (isPrime)

-- This is a modified version of 
-- http://dave.fayr.am/posts/2012-10-4-finding-fizzbuzz.html
-- I added the prime number check, and decided that I could get
-- some improvments from Data.SemiGroup
-- https://www.reddit.com/r/haskell/comments/10zlyy/fizzbuzz_revisited_using_monoids/

-- Here's the gist
-- the comprehensions are type Maybe String
-- Data.SemiGroup imports 
fizzbuzz :: Integer -> Either FizzError String
fizzbuzz i = Right $ fromMaybe (show i) $ getOption fizzbuzz'
  where
    fizzbuzz' = 
      ["fizz " | i `rem` 3 == 0] <>
      ["buzz " | i `rem` 5 == 0] <>
      ["fizzbuzz " | isPrime i]

-- https://wiki.haskell.org/The_Fibonacci_sequence#Constant-time_implementations
fib :: Integer -> Either FizzError Integer
fib n = Right (round $ phi ** fromIntegral n / sq5)
  where
    sq5 = sqrt 5 :: Double
    phi = (1 + sq5) / 2


-- Since Either is a Monad we can eliminate all the case statements
-- we would otherwise have to use.

fizzBuzzFib :: [String] -> Either FizzError [String]
fizzBuzzFib str =
  mapM fizzbuzz          =<<
  mapM fib               =<<
  (\x -> Right [1 .. x]) =<<
  convertToDigit         =<<
  mustHaveOne str
