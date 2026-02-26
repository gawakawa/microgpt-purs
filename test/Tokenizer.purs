module Test.Tokenizer where

import Prelude

import Effect (Effect)
import Tokenizer (buildVocab)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert as Assert

tests :: TestSuite
tests = suite "buildVocab" do
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
