module Test.Train where

import Prelude

import ComputationGraph (mkVal)
import Control.Comonad (extract)
import Control.Monad.Writer (runWriter, tell)
import Data.Array (length)
import Data.Number as N
import Data.Tuple (fst, snd)
import Data.Tuple.Nested (type (/\), (/\))
import Matrix (fromFoldable)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert as Assert
import Train (crossEntropyLoss, cycleN, initDataset, nextTokenPairs)

runInit :: Array String -> Array String /\ Array String
runInit = runWriter <<< initDataset (\msg -> tell [ msg ])

tests :: TestSuite
tests = suite "initDataset" do
  test "empty input returns empty array" do
    Assert.equal [] (fst $ runInit [])
  test "empty strings are removed" do
    Assert.equal [ "foo", "bar" ] (fst $ runInit [ "foo", "", "bar", "" ])
  test "elements and order are preserved" do
    Assert.equal [ "cherry", "apple", "banana" ]
      (fst $ runInit [ "cherry", "apple", "banana" ])
  test "logs the number of remaining docs" do
    Assert.equal [ "num docs: 2" ] (snd $ runInit [ "foo", "", "bar" ])

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

  suite "crossEntropyLoss" do
    test "produces finite loss with extreme logits" do
      -- logits = [1000, 0, 0] -> exp(-1000) underflows to 0
      -- naive implementation: log(0) = -Infinity -> negate = +Infinity (not finite)
      let logits = fromFoldable [ mkVal 1000.0, mkVal 0.0, mkVal 0.0 ]
      let loss = crossEntropyLoss logits 1
      Assert.assert "loss should be finite" $ N.isFinite $ extract loss
