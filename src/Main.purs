module Main where

import Data.List.Lazy
import Prelude

import Control.Apply (lift2)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Array as A
import Data.BigInt (BigInt)
import Data.BigInt as BInt
import Data.Enum (succ)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Int (even, pow)
import Data.List as L
import Data.Maybe (Maybe(..), maybe)
import Data.Natural (Natural, intToNat)
import Data.Newtype (class Newtype)
import Data.Tuple (snd)
import Data.Tuple.Nested ((/\))
import Test.Unit (Test, TestSuite, test)
import Test.Unit.Assert (assert)
import Test.Unit.Console (TESTOUTPUT)
import Test.Unit.Main (runTest)

main :: forall t25.
  Eff
    ( console :: CONSOLE
    , testOutput :: TESTOUTPUT
    , avar :: AVAR
    | t25
    )
    Unit
main = runTest do
  test1
  test2
  test3
  test4

--------------------------------------------------------------------------------
-- 1. Multiples of 3 and 5
--------------------------------------------------------------------------------

multiples :: Natural -> List Natural
multiples n = iterate (_ + n) zero

perform1 :: Natural -> Natural
perform1 limit =
  takeWhile isBelow (multiples (intToNat 3))
  <> takeWhile isBelow (multiples (intToNat 5))
  # nub
  # foldr (+) zero
  where
    isBelow n = n < limit

result1 :: Natural
result1 = perform1 (intToNat 1000)


test1 :: forall eff. TestSuite eff
test1 = test "test1" do
  assert "eq" $ result1 == intToNat 233168

--------------------------------------------------------------------------------
-- 2. Even Fibonacci numbers
--------------------------------------------------------------------------------

fib :: List Int
fib =
  iterate (\(x /\ y) -> (y /\ (x + y))) (1 /\ 1)
  # map snd

perform2 :: Int -> Int
perform2 limit =
  takeWhile (_ < limit) fib
  # filter even
  # foldr (+) zero

result2 :: Int
result2 = perform2 6000000

test2 :: forall eff. TestSuite eff
test2 = test "test2" do
  assert "eq" $ result2 == 4613732

--------------------------------------------------------------------------------
-- 3. Largest prime factor
--------------------------------------------------------------------------------

-- | Prime decomposition using trial division.
-- | Generalized to work not only for integers but for e.g. big ints.
-- |
primeFactors :: forall a.
  Semiring a => Eq a => Ord a => EuclideanRing a => a -> List a
primeFactors n =
  go nil (one + one) n
  where
    go accum divisor m | m == one || divisor >= n =
      reverse accum
    go accum divisor m | m `mod` divisor == zero =
      go (divisor : accum) divisor (m `div` divisor)
    go accum divisor m = go accum (nextDivisor divisor) m

    nextDivisor k =
      if k == one + one then one + one + one else k + one

result3 :: Maybe BigInt
result3 =
  BInt.fromString "600851475143"
  <#> primeFactors
  >>= last

test3 :: forall eff. TestSuite eff
test3 = test "test3" do
  assert "eq" $ result3 == BInt.fromString "6857"

--------------------------------------------------------------------------------
-- 4. Largest palindrome product
--------------------------------------------------------------------------------

type Digits = Array Int

digitsToInt :: Digits -> Int
digitsToInt ns =
  A.reverse ns
  # A.mapWithIndex
    (\i n -> 10 `pow` i * n)
  # A.foldr (+) 0

intToDigits :: Int -> Digits
intToDigits 0 = []
intToDigits n =
  intToDigits (n `div` radix) `A.snoc` (n `mod` radix)
  where
    radix = 10

isPalindrome :: forall a. Eq a => Array a -> Boolean
isPalindrome xs =
  xs == A.reverse xs

perform4 :: Int -> Maybe Int
perform4 n =
  lift2 mul ns ns
  <#> intToDigits
   #  A.filter isPalindrome
  <#> digitsToInt
   #  A.sort
   #  A.last
  where
    ns = A.range (10 `pow` (n-1)) (10 `pow` n-1)

result4 :: Maybe Int
result4 = perform4 3

test4 :: forall eff. TestSuite eff
test4 = test "test4" do
  assert "eq" $ result4 == Just 906609
