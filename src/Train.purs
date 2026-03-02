module Train where

import Prelude

import Control.Comonad (extract)
import Control.Monad.Gen.Trans (Gen, shuffle)
import Data.Array (drop, filter, unsafeIndex, zipWith)
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.List (List)
import Data.Unfoldable (replicate)
import Data.Foldable (class Foldable, foldl, length, sum)
import Data.FoldableWithIndex (foldlWithIndex)
import Data.Int (toNumber)
import Data.Map (lookup)
import Data.Maybe (fromJust, fromMaybe)
import Data.Newtype (unwrap)
import Data.Number as N
import Data.String (Pattern(..), null, split, trim)
import Data.String.CodeUnits (toCharArray)
import Data.Traversable (class Traversable, mapAccumL)
import Data.Tuple (fst)
import Data.Tuple.Nested (type (/\), (/\))
import Partial.Unsafe (unsafePartial)
import Matrix (Vec(..), fromFoldable)
import Params (LayerWeights, StateDict(..))
import GPT (KVCache, gpt)
import Tokenizer (Pos(..), Token(..), tokenize)
import Autograd (GradMap, backward)
import ComputationGraph (class Differentiable, ComputationGraph, exp, fromNumber, log, mkVal)

initDataset :: String -> Gen (Array String)
initDataset content = shuffle docs
  where
  docs = filter (not <<< null) $ trim <$> split (Pattern "\n") content

flatten :: forall t a. Foldable t => t a -> Vec a
flatten = fromFoldable

unflatten :: forall t a b. Traversable t => t a -> Vec b -> t b
unflatten template (Vec vals) = (mapAccumL step vals template).value
  where
  step arr _ = { accum: drop 1 arr, value: unsafePartial $ unsafeIndex arr 0 }

-- | Cycle an array to produce exactly n elements
cycleN :: forall a. Int -> Array a -> Array a
cycleN n arr = Array.take n $ Array.concat $ replicate ((n `div` Array.length arr) + 1) arr

-- | Extract gradient values for parameters from a GradMap
extractGrads :: forall t. Foldable t => t (ComputationGraph Number) -> GradMap -> Vec Number
extractGrads params gradMap = unsafePartial $ (\param -> fromJust $ lookup param gradMap) <$> flatten params

type TrainState =
  { params :: StateDict (ComputationGraph Number)
  , dataset :: Array String
  , m :: Vec Number
  , v :: Vec Number
  , numSteps :: Int
  , learningRate :: Number
  , beta1 :: Number
  , beta2 :: Number
  , epsAdam :: Number
  }

-- | Run training loop for numSteps iterations, cycling through the dataset
train :: TrainState -> TrainState
train state = foldlWithIndex trainStep state $ cycleN state.numSteps state.dataset

-- | Execute one training step: forward pass, backward pass, and Adam update
trainStep :: Int -> TrainState -> String -> TrainState
trainStep step state doc = adamUpdate state step lrT grads
  where
  tokens = tokenize [ doc ]
  loss = forward state.params tokens
  gradMap = backward loss
  grads = extractGrads state.params gradMap
  lrT = state.learningRate * (1.0 - toNumber step / toNumber state.numSteps)

crossEntropyLoss :: forall a. Differentiable a => Vec a -> Int -> a
crossEntropyLoss (Vec logits) targetIdx = negate $ logit - maxVal - log (sum $ exp <<< (_ - maxVal) <$> logits)
  where
  maxVal = foldl max (fromNumber (-N.infinity)) logits
  logit = unsafePartial $ unsafeIndex logits targetIdx

-- | Create (input, target) pairs for next-token prediction training
-- | Returns empty array for inputs with fewer than 2 elements
nextTokenPairs :: forall a. Array a -> Array (a /\ a)
nextTokenPairs tokens = fromMaybe [] do
  inputs <- Array.init tokens
  targets <- Array.tail tokens
  pure $ zipWith (/\) inputs targets

-- | Score next-token prediction at one position: run GPT forward, compare to target
scoreNextToken
  :: StateDict (ComputationGraph Number)
  -> Int
  -> Int
  -> (ComputationGraph Number /\ List (KVCache (ComputationGraph Number)))
  -> (Token /\ Token)
  -> (ComputationGraph Number /\ List (KVCache (ComputationGraph Number)))
scoreNextToken params headDim pos (lossAcc /\ caches) (tok /\ target) =
  lmap (\logits -> lossAcc + crossEntropyLoss logits (unwrap target))
    $ gpt params headDim caches tok (Pos pos)

-- | Forward pass: compute total cross-entropy loss over token sequence
forward :: StateDict (ComputationGraph Number) -> Array Token -> ComputationGraph Number
forward params tokens = fst $ foldlWithIndex (scoreNextToken params sd.headDim) initialState $ nextTokenPairs tokens
  where
  sd = unwrap params
  initialState = zero /\ replicate (length sd.layers) mempty

-- | Update exponential moving averages for gradient moments
updateMoments
  :: { beta1 :: Number, beta2 :: Number }
  -> Vec Number
  -> { m :: Vec Number, v :: Vec Number }
  -> { m :: Vec Number, v :: Vec Number }
updateMoments { beta1, beta2 } grads { m, v } =
  { m: (\mi gi -> beta1 * mi + (1.0 - beta1) * gi) <$> m <*> grads
  , v: (\vi gi -> beta2 * vi + (1.0 - beta2) * gi * gi) <$> v <*> grads
  }

-- | Apply bias correction to moments using (1 - beta^t) divisor
biasCorrect
  :: { beta1 :: Number, beta2 :: Number }
  -> Int
  -> { m :: Vec Number, v :: Vec Number }
  -> { mHat :: Vec Number, vHat :: Vec Number }
biasCorrect { beta1, beta2 } step { m, v } = { mHat, vHat }
  where
  t = toNumber (step + 1)
  mHat = (_ / (1.0 - N.pow beta1 t)) <$> m
  vHat = (_ / (1.0 - N.pow beta2 t)) <$> v

-- | Apply Adam updates to parameters using corrected moments
updateParams
  :: Number
  -> Number
  -> StateDict (ComputationGraph Number)
  -> { mHat :: Vec Number, vHat :: Vec Number }
  -> StateDict (ComputationGraph Number)
updateParams lrT eps params { mHat, vHat } = unflatten params flat'
  where
  updates = (\mh vh -> lrT * mh / (N.sqrt vh + eps)) <$> mHat <*> vHat
  flat' = (\p u -> mkVal $ extract p - u) <$> flatten params <*> updates

adamUpdate :: TrainState -> Int -> Number -> Vec Number -> TrainState
adamUpdate state step lrT grads = state { params = params', m = moments.m, v = moments.v }
  where
  moments = updateMoments { beta1: state.beta1, beta2: state.beta2 } grads { m: state.m, v: state.v }
  params' = updateParams lrT state.epsAdam state.params <<< biasCorrect { beta1: state.beta1, beta2: state.beta2 } step $ moments
