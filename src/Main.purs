module Main where

import Data.List.Lazy
import Prelude

import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.BigInt (BigInt)
import Data.BigInt as BInt
import Data.Enum (succ)
import Data.Int (even)
import Data.Maybe (Maybe, maybe)
import Data.Natural (Natural, intToNat)
import Data.Tuple (snd)
import Data.Tuple.Nested ((/\))
import Test.Unit (Test, test)
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
  test "alltests" do
    test1

--------------------------------------------------------------------------------
-- 1
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

test1 :: forall a. Test a
test1 = assert "test1" $
  result1 == intToNat 233168

--------------------------------------------------------------------------------
-- 2
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

test2 :: forall a. Test a
test2 = assert "test2" $
  result2 == 4613732
