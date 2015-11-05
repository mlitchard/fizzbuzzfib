module Main where

import FizzBuzz
import Control.Applicative ((<$>))
import System.Environment

main :: IO ()
main = do
  input <- getArgs
  let res = 
        case (fizzBuzzFib input) of
          Left err   -> show err
          Right res' -> show res'
  putStrLn res
