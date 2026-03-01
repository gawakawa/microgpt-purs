module Test.Train where

import Prelude

import Control.Monad.Gen.Trans (Gen, evalGen)
import Data.Array (length, replicate, sort)
import Data.String.Common (joinWith)
import Random.LCG (mkSeed)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert as Assert
import Data.Tuple.Nested (type (/\), (/\))
import Train (cycleN, initDataset, nextTokenPairs)

runGen :: forall a. Gen a -> a
runGen gen = evalGen gen { newSeed: mkSeed 42, size: 0 }

tests :: TestSuite
tests = suite "initDataset" do
  test "empty string returns empty array" do
    Assert.equal [] (runGen $ initDataset "")
  test "newlines only returns empty array" do
    Assert.equal [] (runGen $ initDataset "\n\n\n")
  test "blank lines are removed" do
    let result = runGen $ initDataset "foo\n\n  \nbar\n"
    Assert.equal 2 (length result)
  test "deterministic with same seed" do
    let
      input =
        """
        x
        y
        z
        """
      a = runGen $ initDataset input
      b = runGen $ initDataset input
    Assert.equal a b
  test "elements are preserved" do
    let
      input =
        """
        cherry
        apple
        banana
        """
      result = runGen $ initDataset input
    Assert.equal [ "apple", "banana", "cherry" ] (sort result)
  test "large input preserves all elements" do
    let
      input = joinWith "\n" (replicate 32000 "name")
      result = runGen $ initDataset input
    Assert.equal 32000 (length result)

  suite "cycleN" do
    test "returns exactly n elements" do
      Assert.equal 5 $ length $ cycleN 5 [ "a", "b", "c" ]
    test "cycles through array" do
      Assert.equal [ "a", "b", "c", "a", "b" ] $ cycleN 5 [ "a", "b", "c" ]
    test "single element" do
      Assert.equal [ "x", "x", "x" ] $ cycleN 3 [ "x" ]
    test "n equals array length" do
      Assert.equal [ "a", "b" ] $ cycleN 2 [ "a", "b" ]
    test "n less than array length" do
      Assert.equal [ "a" ] $ cycleN 1 [ "a", "b", "c" ]

  suite "nextTokenPairs" do
    test "creates input-target pairs" do
      Assert.equal [ 1 /\ 2, 2 /\ 3, 3 /\ 4 ] $ nextTokenPairs [ 1, 2, 3, 4 ]
    test "two elements" do
      Assert.equal [ 1 /\ 2 ] $ nextTokenPairs [ 1, 2 ]
    test "single element returns empty" do
      Assert.equal ([] :: Array (Int /\ Int)) $ nextTokenPairs [ 1 ]
    test "empty array returns empty" do
      Assert.equal ([] :: Array (Int /\ Int)) $ nextTokenPairs []
