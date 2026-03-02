module Test.Autograd where

import Prelude

import Autograd (GradMap, backward)
import ComputationGraph (mkVal, mkAdd, mkMul, mkPow, mkExp, mkLog, mkRelu)
import Data.Map as Map
import Data.Tuple.Nested ((/\))
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert as Assert

tests :: TestSuite
tests = suite "backward" do
  test "leaf" do
    let
      a = mkVal 5.0
      expected = Map.fromFoldable [ a /\ 1.0 ] :: GradMap
    Assert.equal expected (backward a)

  test "add" do
    let
      a = mkVal 3.0
      b = mkVal 5.0
      node = mkAdd a b
      expected =
        Map.fromFoldable
          [ a /\ 1.0, b /\ 1.0 ] :: GradMap
    Assert.equal expected (backward node)

  test "mul" do
    let
      a = mkVal 3.0
      b = mkVal 5.0
      node = mkMul a b
      expected =
        Map.fromFoldable
          [ a /\ 5.0, b /\ 3.0 ] :: GradMap
    Assert.equal expected (backward node)

  test "pow" do
    let
      a = mkVal 3.0
      node = mkPow a 2.0
      expected =
        Map.fromFoldable
          [ a /\ 6.0 ] :: GradMap
    Assert.equal expected (backward node)

  test "exp" do
    let
      a = mkVal 0.0
      node = mkExp a
      expected =
        Map.fromFoldable
          [ a /\ 1.0 ] :: GradMap
    Assert.equal expected (backward node)

  test "log" do
    let
      a = mkVal 1.0
      node = mkLog a
      expected =
        Map.fromFoldable
          [ a /\ 1.0 ] :: GradMap
    Assert.equal expected (backward node)

  test "relu positive" do
    let
      a = mkVal 5.0
      node = mkRelu a
      expected =
        Map.fromFoldable
          [ a /\ 1.0 ] :: GradMap
    Assert.equal expected (backward node)

  test "relu negative" do
    let
      a = mkVal (-3.0)
      node = mkRelu a
      expected =
        Map.fromFoldable
          [ a /\ 0.0 ] :: GradMap
    Assert.equal expected (backward node)

  test "nested" do
    let
      -- add(mul(a, b), a) where a is shared
      a = mkVal 2.0
      b = mkVal 3.0
      mul = mkMul a b
      root = mkAdd mul a
      -- grad_a: 1.0 (from add right) + 3.0 (from mul, g*b.val=1*3) = 4.0
      -- grad_b: 2.0 (from mul, g*a.val=1*2)
      expected =
        Map.fromFoldable
          [ a /\ 4.0, b /\ 2.0 ] :: GradMap
    Assert.equal expected (backward root)
