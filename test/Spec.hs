import FizzBuzz
import FizzError

import Test.Hspec
import Prelude
import Test.Hspec.QuickCheck
import Data.Either.Utils (fromRight)
import Data.Numbers.Primes (isPrime)
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Clean Input, Correct Control Structure" $ do
    it "returns (NoInput :: FibError)" $
      fizzBuzzFib [] `shouldBe` (Left NoInput)
    it "returns (OnlyOne :: FibError)" $
      fizzBuzzFib ["10","20"] `shouldBe` (Left OnlyOne)
    it "returns (NotAnInteger :: FibError)" $
      fizzBuzzFib buffalo `shouldBe` (Left NotAnInteger)
  describe "Fibonacci check" $
    it "returns a list of integers each wrapped in a Right constructor" $
      map fib [1 .. 10] `shouldBe` fibs
  describe "FizzBuzz check" $
    it "returns proper result according to spec, see test/Spec.hs" $
      fizzBuzzFib ["10"] `shouldBe` fizzBuzzFibs
  describe "QuickCheck test fiz" $
    prop "QuickCheck test" $ modfiz
  describe "QuickCheck test fib" $
    prop "QuickCheck test fib" $ testfib

buffalo = ["Buffalo Buffalo Buffalo Buffalo Buffalo Buffalo Buffalo Buffalo"]

fibs = 
  [Right 1,Right 1,Right 2,Right 3,Right 5,Right 8,Right 13,Right 21,Right 34,Right 55]

fizzBuzzFibs =
  Right ["1","1","boogie down ","fizz boogie down ","buzz boogie down ","8","boogie down ","fizz ","34","buzz "]

modfiz :: Integer -> Bool
modfiz int
  | int <= 0                                 = True
  | int == 3                                 = test3 
  | int == 5                                 = test5
  | int `mod` 15 == 0                        = testMod35
  | int `mod` 3 == 0                         = testMod3
  | int `mod` 5 == 0                         = testMod5 
  | isPrime int == True                      = testPrime 
  | otherwise                                = testRest
      where
        test3     =
          Right "fizz boogie down " == fizzbuzz 3
        test5     =
          Right "buzz boogie down " == fizzbuzz 5
        testMod3  =
          Right "fizz "             == fizzbuzz int
        testMod5  =
          Right "buzz "             == fizzbuzz int
        testMod35 =
          Right "fizz buzz "        == fizzbuzz int
        testPrime =
          Right "boogie down "      == fizzbuzz int
        testRest  =
          Right (show int)          == fizzbuzz int


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

testfib n = 
  case (fib n) of
    Left _ -> False
    Right n' -> isFib n'
 

