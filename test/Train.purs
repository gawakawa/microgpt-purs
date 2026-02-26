module Test.Train where

import Prelude

import Control.Monad.Gen.Trans (Gen, evalGen)
import Data.Array (length, replicate, sort)
import Data.String.Common (joinWith)
import Random.LCG (mkSeed)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert as Assert
import Train (initDataset)

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
