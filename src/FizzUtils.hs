{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE FlexibleContexts #-}
module FizzUtils
    ( ffw
    , bf
    , testfib
    , mod3
    , mod5
    , fizz_prime
    , defaultFib
    ) where

import              FizzTypes
import              FizzBuzz
import              Input
import              Data.Semigroup ((<>),getOption)
import              Data.Maybe (fromMaybe)
import              Data.Numbers.Primes (isPrime)
import              Control.Applicative
ffw :: Fibonator FFW
ffw = Fibonator fib_ffw

bf :: Fibonator BF
bf = Fibonator fib_bf

defaultFib = ffw

fib_ffw n =
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

fib_bf n =
  round $ phi ** fromIntegral n / sq5
  where
    sq5 = sqrt 5 :: Double
    phi = (1 + sq5) / 2

fibPlus (a, b) (c, d) = (bd - (b - a)*(d - c), a*c + bd)
  where bd = b*d

unFib (a, b) n
  | n < a = (0, 0, 1)
  | n < e = (2*k, c, d)
  | otherwise = (2*k + 1, e, f)
      where
        (k, c, d) = unFib (fibPlus (a, b) (a, b)) n
        (e, f)    = fibPlus (a, b) (c, d)

isFib n = n == a where (k, a, b) = unFib (1, 1) n

--mod3 :: ((Integral a) => (a -> Maybe String)) 
mod3,mod5,fizz_prime :: (Integer -> Maybe String)
mod3 i       = ["fizz "        | i `rem` 3 == 0]
mod5 i       = ["buzz "        | i `rem` 5 == 0]
fizz_prime i = ["boogie down " | isPrime i     ]

startOptions :: Options
startOptions = Options
  { fib_function = defaultFib
  , fizz_comp    = Fizzanator (mod3 <> mod5 <> fizz_prime)
  }

options :: [ OptDescr (Options -> Options) ]
options =
  [ Option "
testfib fcn n =
  let (Right fib_res) = fibb fcn n
  in isFib fib_res
      
