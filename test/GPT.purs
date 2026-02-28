module Test.GPT where

import Prelude

import Data.Array (unsafeIndex)
import Data.Array as Array
import Data.Foldable (all, sum)
import Data.Int (toNumber)
import Data.Number (sqrt)
import Data.Number.Approximate ((≅))
import GPT (attention, embed, mlp, rmsnorm, softmax)
import Partial.Unsafe (unsafePartial)
import Matrix (Vec(..), dot, fromFoldable)
import Params (LayerWeights(..))
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert as Assert

rms :: Vec Number -> Number
rms v@(Vec arr) = sqrt $ dot v v / toNumber (Array.length arr)

tests :: TestSuite
tests = suite "GPT" do
  suite "embed" do
    test "returns first row" do
      let matrix = Vec [Vec [1, 2, 3], Vec [4, 5, 6]]
      Assert.equal (Vec [1, 2, 3]) (embed matrix 0)

    test "returns second row" do
      let matrix = Vec [Vec [1, 2, 3], Vec [4, 5, 6]]
      Assert.equal (Vec [4, 5, 6]) (embed matrix 1)

  suite "softmax" do
    test "output sums to 1.0" do
      let input = Vec [1.0, 2.0, 3.0]
      let result = softmax input
      Assert.assert "sum ≈ 1" $ sum result ≅ 1.0

    test "all outputs are non-negative" do
      let input = Vec [-1.0, 0.0, 1.0]
      let result = softmax input
      Assert.assert "all non-negative" $ all (_ >= 0.0) result

    test "larger input produces larger probability" do
      let input = Vec [1.0, 2.0, 3.0]
      let Vec arr = softmax input
      let p1 = unsafePartial $ unsafeIndex arr 0
      let p2 = unsafePartial $ unsafeIndex arr 1
      let p3 = unsafePartial $ unsafeIndex arr 2
      Assert.assert "p1 < p2" $ p1 < p2
      Assert.assert "p2 < p3" $ p2 < p3

    test "uniform input produces uniform output" do
      let input = Vec [1.0, 1.0, 1.0]
      let Vec arr = softmax input
      let p1 = unsafePartial $ unsafeIndex arr 0
      let p2 = unsafePartial $ unsafeIndex arr 1
      let p3 = unsafePartial $ unsafeIndex arr 2
      Assert.assert "p1 ≈ p2" $ p1 ≅ p2
      Assert.assert "p2 ≈ p3" $ p2 ≅ p3

  suite "rmsnorm" do
    test "output has RMS ≈ 1.0" do
      let input = Vec [1.0, 2.0, 3.0, 4.0]
      let result = rmsnorm input
      Assert.assert "RMS ≈ 1" $ rms result ≅ 1.0

    test "works with single element" do
      let input = Vec [5.0]
      let result = rmsnorm input
      Assert.assert "RMS ≈ 1" $ rms result ≅ 1.0

  suite "mlp" do
    test "zero weights produce zero output" do
      let
        z = Vec [Vec [0.0, 0.0], Vec [0.0, 0.0]]
        w = LayerWeights { attnWq: z, attnWk: z, attnWv: z, attnWo: z, mlpFc1: z, mlpFc2: z }
      Assert.equal (Vec [0.0, 0.0]) $ mlp w (Vec [1.0, 2.0])

    test "output dimension matches input dimension" do
      let
        z = Vec [Vec [0.0, 0.0], Vec [0.0, 0.0]]
        w = LayerWeights { attnWq: z, attnWk: z, attnWv: z, attnWo: z, mlpFc1: z, mlpFc2: z }
        input = Vec [1.0, 2.0]
        Vec result = mlp w input
        Vec inputArr = input
      Assert.equal (Array.length inputArr) $ Array.length result

  suite "attention" do
    test "matching key gets high attention weight" do
      let
        query = Vec [1.0, 0.0]
        keys = fromFoldable [Vec [1.0, 0.0], Vec [0.0, 1.0]]
        values = fromFoldable [Vec [1.0, 0.0], Vec [0.0, 1.0]]
        Vec result = attention query keys values
        v1 = unsafePartial $ unsafeIndex result 0
        v2 = unsafePartial $ unsafeIndex result 1
      Assert.assert "first component higher" $ v1 > v2

    test "orthogonal keys get low attention" do
      let
        query = Vec [1.0, 0.0]
        keys = fromFoldable [Vec [0.0, 1.0]]
        values = fromFoldable [Vec [1.0, 0.0]]
        result = attention query keys values
      Assert.equal (Vec [1.0, 0.0]) result

    test "output dimension matches query dimension" do
      let
        query = Vec [1.0, 2.0, 3.0]
        keys = fromFoldable [Vec [1.0, 0.0, 0.0], Vec [0.0, 1.0, 0.0]]
        values = fromFoldable [Vec [1.0, 2.0, 3.0], Vec [4.0, 5.0, 6.0]]
        Vec result = attention query keys values
        Vec queryArr = query
      Assert.equal (Array.length queryArr) $ Array.length result
