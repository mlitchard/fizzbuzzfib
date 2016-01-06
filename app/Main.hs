{-# LANGUAGE Rank2Types #-}

module Main where

import              FizzBuzz
import              FizzTypes
import              FizzUtils 
import              Control.Applicative ((<$>))
import              System.Environment ( getArgs )
import              System.Console.GetOpt
import              Data.Maybe (fromMaybe)
import              Control.Applicative
--main :: IO ()
--main = do
--  input <- getArgs
-- putStrLn $ either show (unlines . (map show)) $ fizzBuzzFib input defaultFib


main :: IO ()
main = do
  input <- getArgs
  return ()


