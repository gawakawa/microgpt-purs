module Test.Autograd where

import Prelude

import Autograd (GradMap, backward, buildDag)
import ComputationGraph (ComputationGraph(..))
import Data.Map as Map
import Data.Tuple.Nested ((/\))
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert as Assert

tests :: TestSuite
tests = suite "backward" do
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
