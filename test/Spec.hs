import FizzBuzz
import FizzError

import Test.Hspec

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

buffalo = ["Buffalo Buffalo Buffalo Buffalo Buffalo Buffalo Buffalo Buffalo"]

fibs = 
  [Right 1,Right 1,Right 2,Right 3,Right 5,Right 8,Right 13,Right 21,Right 34,Right 55]

fizzBuzzFibs =
  Right ["1","1","boogie down ","fizz boogie down ","buzz boogie down ","8","boogie down ","fizz ","34","buzz "]
