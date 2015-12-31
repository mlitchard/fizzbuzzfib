{-# LANGUAGE MonadComprehensions #-}
module FizzBuzz
    (fizzbuzz
    ,fib 
    ,fizzBuzzFib
    ,fib_gen_1
    ,fib_gen_2
    ,defaultFib
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
      ["fizz "        | i `rem` 3 == 0] <>
      ["buzz "        | i `rem` 5 == 0] <>
      ["bang!"        | isPrime i     ]

-- https://wiki.haskell.org/The_Fibonacci_sequence#Constant-time_implementations
--fib :: Integer -> Either FizzError Integer
--fib n = Right $ (round $ phi ** fromIntegral n / sq5)
--  where
--    sq5 = sqrt 5 :: Double
--    phi = (1 + sq5) / 2

fib :: (Integer -> Integer) -> Integer -> Either FizzError Integer
fib f n = 
  Right (f n)

defaultFib = fib_gen_1

fib_gen_1 n = 
  snd . foldl fib' (1, 0) . map (toEnum . fromIntegral) $ unfoldl divs n
    where
      unfoldl f x = 
        case f x of
          Nothing     -> []
          Just (u, v) -> unfoldl f v ++ [u]
 
      divs 0 = Nothing
      divs k = Just (uncurry (flip (,)) (k `divMod` 2))
 
      fib' (f, g) p
        | p         = (f*(f+2*g), f^2 + g^2)
        | otherwise = (f^2+g^2,   g*(2*f-g))

fib_gen_2 n =
  round $ phi ** fromIntegral n / sq5
    where
      sq5 = sqrt 5 :: Double
      phi = (1 + sq5) / 2

-- Since Either is a Monad we can eliminate all the case statements
-- we would otherwise have to use.

fizzBuzzFib :: (Integer -> Integer) -> [String] -> Either FizzError [String]
fizzBuzzFib f str =
  mapM fizzbuzz          =<<
  mapM (fib f)           =<<
  (\x -> Right [1 .. x]) =<<
  convertToNatural       =<<
  mustHaveOne str
