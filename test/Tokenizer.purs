module Test.Tokenizer where

import Prelude

import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert as Assert
import Data.Maybe (Maybe(..))
import Tokenizer (Token(..), bos, buildVocab, decode, encode, tokenize)

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
      Assert.equal (Token <$> [ 26 ]) (tokenize [])
    test "single character doc" do
      Assert.equal (Token <$> [ 26, 0, 26 ]) (tokenize [ "a" ])
    test "single doc with multiple chars" do
      Assert.equal (Token <$> [ 26, 0, 1, 26 ]) (tokenize [ "ab" ])
    test "multiple docs" do
      Assert.equal (Token <$> [ 26, 0, 1, 26, 1, 2, 26 ]) (tokenize [ "ab", "bc" ])
    test "duplicate chars are deduplicated in vocab" do
      Assert.equal (Token <$> [ 26, 0, 0, 1, 26 ]) (tokenize [ "aab" ])
    test "vocab is sorted alphabetically" do
      Assert.equal (Token <$> [ 26, 2, 0, 26 ]) (tokenize [ "ca" ])

  suite "encode" do
    test "'a' encodes to Token 0" do
      Assert.equal (Token 0) (encode 'a')
    test "'b' encodes to Token 1" do
      Assert.equal (Token 1) (encode 'b')
    test "'z' encodes to Token 25" do
      Assert.equal (Token 25) (encode 'z')
    test "'A' encodes to Token -32" do
      -- FIXME: restrict domain
      Assert.equal (Token (-32)) (encode 'A')

  suite "decode" do
    test "Token 0 decodes to Just 'a'" do
      Assert.equal (Just 'a') (decode (Token 0))
    test "Token 25 decodes to Just 'z'" do
      Assert.equal (Just 'z') (decode (Token 25))
    test "bos decodes to Nothing" do
      Assert.equal Nothing (decode bos)
    test "Token -1 decodes to Just '`'" do
      -- FIXME: restrict domain
      Assert.equal (Just '`') (decode (Token (-1)))
    test "Token 27 decodes to Just '|'" do
      -- FIXME: restrict domain
      Assert.equal (Just '|') (decode (Token 27))
