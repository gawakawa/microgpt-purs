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
      Assert.equal [ 26 ] (tokenize [])
    test "single character doc" do
      Assert.equal [ 26, 0, 26 ] (tokenize [ "a" ])
    test "single doc with multiple chars" do
      Assert.equal [ 26, 0, 1, 26 ] (tokenize [ "ab" ])
    test "multiple docs" do
      Assert.equal [ 26, 0, 1, 26, 1, 2, 26 ] (tokenize [ "ab", "bc" ])
    test "duplicate chars are deduplicated in vocab" do
      Assert.equal [ 26, 0, 0, 1, 26 ] (tokenize [ "aab" ])
    test "vocab is sorted alphabetically" do
      Assert.equal [ 26, 2, 0, 26 ] (tokenize [ "ca" ])
