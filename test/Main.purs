module Test.Main where

import Prelude

import Effect (Effect)
import Main (tokenize)
import Test.Unit (suite, test)
import Test.Unit.Main (runTest)
import Test.Unit.Assert as Assert

main :: Effect Unit
main = runTest do
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
