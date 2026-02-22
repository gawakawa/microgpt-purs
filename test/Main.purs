module Test.Main where

import Prelude

import Data.Array (length, replicate, sort)
import Data.Map as Map
import Data.String.Common (joinWith)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Graph.Weighted (fromEdges)
import Data.Graph.Weighted.DAG (DAG, unsafeFromWeightedDigraph)
import Effect (Effect)
import Main (Expr(..), GradMap, backward, initDataset, tokenize)
import Random.LCG (Seed, mkSeed)
import Test.Unit (suite, test)
import Test.Unit.Main (runTest)
import Test.Unit.Assert as Assert

buildDag :: Expr Number -> DAG (Expr Number) Unit
buildDag root = unsafeFromWeightedDigraph $ fromEdges (collectEdges root)
  where
  collectEdges :: Expr Number -> Array { source :: Expr Number, target :: Expr Number, weight :: Unit }
  collectEdges expr = case expr of
    Val _ -> []
    Add _ a b ->
      [ { source: expr, target: a, weight: unit }
      , { source: expr, target: b, weight: unit }
      ] <> collectEdges a <> collectEdges b
    Mul _ a b ->
      [ { source: expr, target: a, weight: unit }
      , { source: expr, target: b, weight: unit }
      ] <> collectEdges a <> collectEdges b
    Pow _ a _ ->
      [ { source: expr, target: a, weight: unit }
      ] <> collectEdges a
    Exp _ a ->
      [ { source: expr, target: a, weight: unit }
      ] <> collectEdges a
    Log _ a ->
      [ { source: expr, target: a, weight: unit }
      ] <> collectEdges a
    Relu _ a ->
      [ { source: expr, target: a, weight: unit }
      ] <> collectEdges a

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

  suite "backward" do
    test "leaf" do
      let
        a = Val 5.0
        dag = buildDag a
        expected = Map.fromFoldable [ a /\ 1.0 ] :: GradMap
      Assert.equal expected (backward a dag)

    test "add" do
      let
        a = Val 3.0
        b = Val 5.0
        node = Add 8.0 a b
        dag = buildDag node
        expected =
          Map.fromFoldable
            [ a /\ 1.0, b /\ 1.0, node /\ 1.0 ] :: GradMap
      Assert.equal expected (backward node dag)

    test "mul" do
      let
        a = Val 3.0
        b = Val 5.0
        node = Mul 15.0 a b
        dag = buildDag node
        expected =
          Map.fromFoldable
            [ a /\ 5.0, b /\ 3.0, node /\ 1.0 ] :: GradMap
      Assert.equal expected (backward node dag)

    test "pow" do
      let
        a = Val 3.0
        node = Pow 9.0 a 2.0
        dag = buildDag node
        expected =
          Map.fromFoldable
            [ a /\ 6.0, node /\ 1.0 ] :: GradMap
      Assert.equal expected (backward node dag)

    test "exp" do
      let
        a = Val 0.0
        node = Exp 1.0 a
        dag = buildDag node
        expected =
          Map.fromFoldable
            [ a /\ 1.0, node /\ 1.0 ] :: GradMap
      Assert.equal expected (backward node dag)

    test "log" do
      let
        a = Val 1.0
        node = Log 0.0 a
        dag = buildDag node
        expected =
          Map.fromFoldable
            [ a /\ 1.0, node /\ 1.0 ] :: GradMap
      Assert.equal expected (backward node dag)

    test "relu positive" do
      let
        a = Val 5.0
        node = Relu 5.0 a
        dag = buildDag node
        expected =
          Map.fromFoldable
            [ a /\ 1.0, node /\ 1.0 ] :: GradMap
      Assert.equal expected (backward node dag)

    test "relu negative" do
      let
        a = Val (-3.0)
        node = Relu 0.0 a
        dag = buildDag node
        expected =
          Map.fromFoldable
            [ a /\ 0.0, node /\ 1.0 ] :: GradMap
      Assert.equal expected (backward node dag)

    test "nested" do
      let
        -- add(mul(a, b), a) where a is shared
        a = Val 2.0
        b = Val 3.0
        mul = Mul 6.0 a b
        root = Add 8.0 mul a
        dag = buildDag root
        -- grad_a: 1.0 (from add right) + 3.0 (from mul, g*b.val=1*3) = 4.0
        -- grad_b: 2.0 (from mul, g*a.val=1*2)
        expected =
          Map.fromFoldable
            [ a /\ 4.0, b /\ 2.0, mul /\ 1.0, root /\ 1.0 ] :: GradMap
      Assert.equal expected (backward root dag)
