module Test.Tokenizer where

import Prelude

import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert as Assert
import Tokenizer (buildVocab, tokenize)

tests :: TestSuite
tests = do
  suite "buildVocab" do
    test "empty input returns empty array" do
      Assert.equal [] (buildVocab [])
    test "single char" do
      Assert.equal [ 'a' ] (buildVocab [ "a" ])
    test "multiple chars are sorted" do
      Assert.equal [ 'a', 'b', 'c' ] (buildVocab [ "cba" ])
    test "duplicates are removed" do
      Assert.equal [ 'a', 'b' ] (buildVocab [ "aabb" ])
    test "multiple docs are merged" do
      Assert.equal [ 'a', 'b', 'c' ] (buildVocab [ "ab", "bc" ])

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
