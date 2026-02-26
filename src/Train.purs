module Train where

import Prelude

import Control.Comonad (extract)
import Control.Monad.Gen.Trans (Gen, shuffle)
import Data.Array (drop, filter, fromFoldable, length, replicate, slice, unsafeIndex, zipWith)
import Data.Foldable (class Foldable)
import Data.FoldableWithIndex (foldlWithIndex)
import Data.Int (toNumber)
import Data.Map (lookup)
import Data.Maybe (fromJust)
import Data.Newtype (unwrap)
import Data.Number as N
import Data.String (Pattern(..), null, split, trim)
import Data.String.CodeUnits (toCharArray)
import Data.Traversable (class Traversable, mapAccumL)
import Data.Tuple.Nested (type (/\), (/\))
import Partial.Unsafe (unsafePartial)
import Params (LayerWeights, StateDict(..))
import GPT (TokenId(..), PosId(..), softmax, gpt)
import Tokenizer (buildVocab, tokenize)
import Autograd (GradMap, backward, buildDag)
import ComputationGraph (class Differentiable, ComputationGraph(..), log)

initDataset :: String -> Gen (Array String)
initDataset content = shuffle docs
  where
  docs = filter (not <<< null) $ trim <$> split (Pattern "\n") content

flatten :: forall t a. Foldable t => t a -> Array a
flatten = fromFoldable

unflatten :: forall t a b. Traversable t => t a -> Array b -> t b
unflatten template vals = (mapAccumL step vals template).value
  where
  step arr _ = { accum: drop 1 arr, value: unsafePartial $ unsafeIndex arr 0 }

type TrainState =
  { params :: StateDict (ComputationGraph Number)
  , dataset :: Array String
  , m :: Array Number
  , v :: Array Number
  , numSteps :: Int
  , learningRate :: Number
  , beta1 :: Number
  , beta2 :: Number
  , epsAdam :: Number
  }

train :: TrainState -> Int -> TrainState
train state step = state { params = params', m = m', v = v' }
  where
  doc = unsafePartial $ unsafeIndex state.dataset (step `mod` length state.dataset)
  tokens = tokenize [ doc ]
  loss = forward state.params tokens
  grads = unsafePartial $ (\p -> fromJust $ lookup p gradMap) <$> flatten state.params
  dag = buildDag loss
  gradMap = backward loss dag
  lrT = state.learningRate * (1.0 - toNumber step / toNumber state.numSteps)
  params' /\ m' /\ v' = adamUpdate state step lrT grads

crossEntropyLoss :: forall a. Differentiable a => Array a -> Int -> a
crossEntropyLoss logits targetIdx = negate $ log prob
  where
  probs = softmax logits
  prob = unsafePartial $ unsafeIndex probs targetIdx

forward :: StateDict (ComputationGraph Number) -> Array Int -> ComputationGraph Number
forward params tokens = totalLoss
  where
  sd = unwrap params
  nLayer = length sd.layers
  initialCaches = replicate nLayer mempty
  inputs = slice 0 (length tokens - 1) tokens
  targets = slice 1 (length tokens) tokens

  step pos (caches /\ lossAcc) (tok /\ target) = caches' /\ (lossAcc + loss)
    where
    logits /\ caches' = gpt params sd.headDim caches (TokenId tok) (PosId pos)
    loss = crossEntropyLoss logits target

  _ /\ totalLoss = foldlWithIndex step (initialCaches /\ zero) (zipWith (/\) inputs targets)

adamUpdate :: TrainState -> Int -> Number -> Array Number -> StateDict (ComputationGraph Number) /\ Array Number /\ Array Number
adamUpdate state step lrT grads = params' /\ m' /\ v'
  where
  t = toNumber (step + 1)

  -- Moment updates
  m' = zipWith (\mi gi -> state.beta1 * mi + (1.0 - state.beta1) * gi) state.m grads
  v' = zipWith (\vi gi -> state.beta2 * vi + (1.0 - state.beta2) * gi * gi) state.v grads

  -- Bias-corrected moments
  mHat = (_ / (1.0 - N.pow state.beta1 t)) <$> m'
  vHat = (_ / (1.0 - N.pow state.beta2 t)) <$> v'

  -- Parameter updates
  updates = zipWith (\mh vh -> lrT * mh / (N.sqrt vh + state.epsAdam)) mHat vHat

  flat = flatten state.params
  flat' = zipWith (\p u -> Val $ extract p - u) flat updates
  params' = unflatten state.params flat'
