--{-# LANGUAGE Rank2Types #-}
--{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import              FizzBuzz
import              FizzTypes
import              FizzUtils 
import              Control.Applicative ((<$>))
import              System.Environment ( getArgs )
import              System.Console.GetOpt
import              Data.Maybe (fromMaybe)
import              Control.Applicative
--import              System.Console.CmdArgs
--main :: IO ()
--main = do
--  input <- getArgs
-- putStrLn $ either show (unlines . (map show)) $ fizzBuzzFib input defaultFib


main :: IO ()
main = do
--  print =<< cmdArgs defaultArgs
--  input <- getArgs
--  print =<< cmdArgs input
  return ()




--main = print =<< cmdArgs sample
