# fizzbuzzfib
Fizz Buzz, based on the following blog article : http://dave.fayr.am/posts/2012-10-4-finding-fizzbuzz.html

So where are the changes?

To the fizzbuzz function itself:

Not much, the use of monad comprehensions 
in the above blog entry, and the fact that Option (Maybe in the blog entry) forms a
 Monoid allowed me to add a new feature easily.

Using Maybe wrapped in Option. This is because Option forms a monoid in a cleaner way. It cost very little time to add, and my intuition tells me it's always
better to select the mathematically correct approach when the cost to do so is so low.

Function signature -
fizzbuzz :: Integer -> Either FibError String

This is to accomodate the input checks I do prior.

That's where most of the work happened, checking input. It begins with a sum
type

data FibError
  = NotAnInteger
  | OnlyOne
  | NoInput

which represents the different ways the input checks can fail: when the user
passes a non-integer, multiple strings, or nothing at all.

There's not much to say about `onlyOne` and `convertToDigit`,
but I will comment about ``fizzBuzzFib`.

The major feature of this driver function is that it leverages the Either Monad.

Without this property, Either would require a staircase of case statements.
In this function, it would only be 2 levels deep, but still - yuck.

HowTo Install

Without Docker -

1) Install stack, if it's absent - https://github.com/commercialhaskell/stack#how-to-install

2) git clone https://github.com/mlitchard/fizzbuzzfib.git

3) cd fizzbuzzfib && stack setup - stack setup will insure you are using the ghc fizzbuzzfib expects

4) stack build

5) /home/$HOME/fizzbuzzfib/.stack-work/install/x86_64-linux/lts-2.19/7.8.4/bin
