module Main where

import FizzBuzz
import Control.Applicative ((<$>))
import System.Environment

main :: IO ()
main = do
  input <- getArgs
  putStrLn $ either show (unlines . (map show)) $ fizzBuzzFib input
