module Test.Main where

import Prelude

import Data.Array (length, replicate, sort)
import Data.String.Common (joinWith)
import Effect (Effect)
import Main (initDataset, tokenize)
import Random.LCG (Seed, mkSeed)
import Test.Unit (suite, test)
import Test.Unit.Main (runTest)
import Test.Unit.Assert as Assert

seed :: Seed
seed = mkSeed 42

main :: Effect Unit
main = runTest do
  suite "initDataset" do
    test "empty string returns empty array" do
      Assert.equal [] (initDataset seed "")
    test "newlines only returns empty array" do
      Assert.equal [] (initDataset seed "\n\n\n")
    test "blank lines are removed" do
      let result = initDataset seed "foo\n\n  \nbar\n"
      Assert.equal 2 (length result)
    test "deterministic with same seed" do
      let
        input =
          """
          x
          y
          z
          """
        a = initDataset seed input
        b = initDataset seed input
      Assert.equal a b
    test "elements are preserved" do
      let
        input =
          """
          cherry
          apple
          banana
          """
        result = initDataset seed input
      Assert.equal [ "apple", "banana", "cherry" ] (sort result)
    test "large input preserves all elements" do
      let
        input = joinWith "\n" (replicate 32000 "name")
        result = initDataset seed input
      Assert.equal 32000 (length result)
  suite "tokenize" do
    test "empty input produces single BOS" do
      Assert.equal [ 0 ] (tokenize [])
    test "single character doc" do
      Assert.equal [ 1, 0, 1 ] (tokenize [ "a" ])
    test "single doc with multiple chars" do
      Assert.equal [ 2, 0, 1, 2 ] (tokenize [ "ab" ])
    test "multiple docs" do
      Assert.equal [ 3, 0, 1, 3, 1, 2, 3 ] (tokenize [ "ab", "bc" ])
    test "duplicate chars are deduplicated in vocab" do
      Assert.equal [ 2, 0, 0, 1, 2 ] (tokenize [ "aab" ])
    test "vocab is sorted alphabetically" do
      Assert.equal [ 2, 2, 0, 2 ] (tokenize [ "ca" ])
