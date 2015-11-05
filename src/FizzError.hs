module FizzError
  (FizzError (..)) where


-- FizzBuzzFib can fail with bad input three ways: 
--   if you gave it input that wasn't an integer,
--   You fed in more that one piece of data,
--   or you didn't provide any data at all
--   FibError captures this

data FizzError
  = NotAnInteger
  | OnlyOne
  | NoInput
    deriving Eq

instance Show FizzError where
  show NotAnInteger = "not an integer"
  show OnlyOne      = "Just pass in one number that decribes how many" ++
                      " fibonacci numbers you want for fizzbuzz."
  show NoInput      = "You need to pass in an integer that describes how" ++
                      " many fibonacci numbers you want for fizzbuzz."

