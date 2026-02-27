module Test.GPT where

import Prelude

import GPT (embed)
import Matrix (Vec(..))
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert as Assert

tests :: TestSuite
tests = suite "GPT" do
  suite "embed" do
    test "returns first row" do
      let matrix = [[1, 2, 3], [4, 5, 6]]
      Assert.equal (Vec [1, 2, 3]) (embed matrix 0)

    test "returns second row" do
      let matrix = [[1, 2, 3], [4, 5, 6]]
      Assert.equal (Vec [4, 5, 6]) (embed matrix 1)
