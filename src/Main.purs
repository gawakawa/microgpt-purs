module Main where

import Prelude

import Control.Comonad (class Comonad, class Extend, extend, extract)
import Control.Monad.Gen.Class (class MonadGen, chooseFloat)
import Control.Monad.Gen.Trans (Gen, evalGen, shuffle)
import Data.Array (drop, filter, fromFoldable, length, range, replicate, slice, unsafeIndex, zipWith)
import Data.Int (toNumber)
import Data.Char (toCharCode)
import Data.Foldable (class Foldable, foldl, foldMap, foldr, sum, surroundMap)
import Data.FoldableWithIndex (foldlWithIndex)
import Data.Traversable (class Traversable, traverse, mapAccumL)
import Data.Function (on)
import Data.Graph.Weighted (fromEdges)
import Data.Graph.Weighted.DAG (DAG, topologicalSort, unsafeFromWeightedDigraph)
import Data.Map (Map, fromFoldableWith, lookup, singleton, unionWith)
import Data.Maybe (Maybe(..), fromJust, fromMaybe)
import Data.Number as N
import Data.String (Pattern(..), null, split, trim)
import Data.String.CodeUnits (toCharArray)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Unfoldable (replicateA)
import Data.Newtype (unwrap)
import Partial.Unsafe (unsafePartial)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import Random.LCG (randomSeed)
import Inference (class Differentiable, fromNumber, exp, log, pow, relu, Matrix, LayerWeights(..), StateDict(..), TokenId(..), PosId(..), KVCache, buildVocab, softmax, gpt, inference)

data ComputationGraph a
  = Val a
  | Add a (ComputationGraph a) (ComputationGraph a)
  | Mul a (ComputationGraph a) (ComputationGraph a)
  | Pow a (ComputationGraph a) Number
  | Exp a (ComputationGraph a)
  | Log a (ComputationGraph a)
  | Relu a (ComputationGraph a)

derive instance Eq a => Eq (ComputationGraph a)
derive instance Ord a => Ord (ComputationGraph a)
derive instance Functor ComputationGraph

instance Semiring (ComputationGraph Number) where
  zero = Val 0.0
  one = Val 1.0
  add a b = Add (extract a + extract b) a b
  mul a b = Mul (extract a * extract b) a b

instance Ring (ComputationGraph Number) where
  sub a b = add a (mul (Val (-1.0)) b)

instance CommutativeRing (ComputationGraph Number)

instance EuclideanRing (ComputationGraph Number) where
  degree _ = 1
  div a b = mul a (Pow (1.0 / extract b) b (-1.0))
  mod _ _ = zero

instance Differentiable (ComputationGraph Number) where
  exp a = Exp (N.exp $ extract a) a
  log a = Log (N.log $ extract a) a
  pow a n = Pow (N.pow (extract a) n) a n
  relu a = Relu (max 0.0 $ extract a) a
  fromNumber = Val

instance Show a => Show (ComputationGraph a) where
  show (Val v) = "(Val " <> show v <> ")"
  show (Add v a b) = "(Add " <> show v <> " " <> show a <> " " <> show b <> ")"
  show (Mul v a b) = "(Mul " <> show v <> " " <> show a <> " " <> show b <> ")"
  show (Pow v a n) = "(Pow " <> show v <> " " <> show a <> " " <> show n <> ")"
  show (Exp v a) = "(Exp " <> show v <> " " <> show a <> ")"
  show (Log v a) = "(Log " <> show v <> " " <> show a <> ")"
  show (Relu v a) = "(Relu " <> show v <> " " <> show a <> ")"

instance Extend ComputationGraph where
  extend f expr@(Val _) = Val (f expr)
  extend f expr@(Add _ a b) = Add (f expr) (extend f a) (extend f b)
  extend f expr@(Mul _ a b) = Mul (f expr) (extend f a) (extend f b)
  extend f expr@(Pow _ a n) = Pow (f expr) (extend f a) n
  extend f expr@(Exp _ a) = Exp (f expr) (extend f a)
  extend f expr@(Log _ a) = Log (f expr) (extend f a)
  extend f expr@(Relu _ a) = Relu (f expr) (extend f a)

instance Comonad ComputationGraph where
  extract (Val v) = v
  extract (Add v _ _) = v
  extract (Mul v _ _) = v
  extract (Pow v _ _) = v
  extract (Exp v _) = v
  extract (Log v _) = v
  extract (Relu v _) = v

type GradMap = Map (ComputationGraph Number) Number

propagate :: Number -> ComputationGraph Number -> Array (ComputationGraph Number /\ Number)
propagate g = case _ of
  Val _ -> []
  -- ∂(a+b)/∂a = 1, ∂(a+b)/∂b = 1
  Add _ a b -> [ a /\ g, b /\ g ]
  -- ∂(a·b)/∂a = b, ∂(a·b)/∂b = a
  Mul _ a b -> [ a /\ (g * extract b), b /\ (g * extract a) ]
  -- ∂aⁿ/∂a = n·aⁿ⁻¹
  Pow _ a n -> [ a /\ (g * n * N.pow (extract a) (n - 1.0)) ]
  -- ∂eᵃ/∂a = eᵃ
  Exp v a -> [ a /\ (g * v) ]
  -- ∂(ln a)/∂a = 1/a
  Log _ a -> [ a /\ (g / extract a) ]
  -- ∂max(0,a)/∂a = 1 if a>0, else 0
  Relu _ a -> [ a /\ (g * if extract a > 0.0 then 1.0 else 0.0) ]

backward :: ComputationGraph Number -> DAG (ComputationGraph Number) Unit -> GradMap
backward root dag = foldl step (singleton root 1.0) (topologicalSort dag)
  where
  step :: GradMap -> ComputationGraph Number -> GradMap
  step grads expr = fromMaybe grads do
    g <- lookup expr grads
    pure $ unionWith (+) grads (fromFoldableWith (+) $ propagate g expr)

buildDag :: ComputationGraph Number -> DAG (ComputationGraph Number) Unit
buildDag root = unsafeFromWeightedDigraph $ fromEdges (collectEdges root)
  where
  collectEdges :: ComputationGraph Number -> Array { source :: ComputationGraph Number, target :: ComputationGraph Number, weight :: Unit }
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

encode :: Char -> Int
encode c = on (-) toCharCode c 'a'

tokenize :: Array String -> Array Int
tokenize docs = surroundMap [ bos ] (map encode <<< toCharArray) docs
  where
  bos = length $ buildVocab docs

initDataset :: String -> Gen (Array String)
initDataset content = shuffle docs
  where
  docs = filter (not <<< null) $ trim <$> split (Pattern "\n") content

sampleGauss :: forall m. MonadGen m => Number -> m Number
sampleGauss std = do
  u1 <- chooseFloat 1.0e-7 1.0
  u2 <- chooseFloat 0.0 1.0
  let z = N.sqrt (-2.0 * N.log u1) * N.cos (2.0 * N.pi * u2)
  pure $ z * std

matrix :: Int -> Int -> Gen (Matrix (ComputationGraph Number))
matrix nout nin = replicateA nout $ replicateA nin do
  g <- sampleGauss std
  pure $ Val g
  where
  std = 0.08

initParams :: Int -> Int -> Int -> Int -> Int -> Gen (StateDict (ComputationGraph Number))
initParams nEmbd nHead nLayer blockSize vocabSize = do
  wte <- matrix vocabSize nEmbd
  wpe <- matrix blockSize nEmbd
  lmHead <- matrix vocabSize nEmbd
  layers <- replicateA nLayer do
    attnWq <- matrix nEmbd nEmbd
    attnWk <- matrix nEmbd nEmbd
    attnWv <- matrix nEmbd nEmbd
    attnWo <- matrix nEmbd nEmbd
    mlpFc1 <- matrix (4 * nEmbd) nEmbd
    mlpFc2 <- matrix nEmbd (4 * nEmbd)
    pure $ LayerWeights { attnWq, attnWk, attnWv, attnWo, mlpFc1, mlpFc2 }
  pure $ StateDict { wte, wpe, lmHead, layers, headDim }
  where
  headDim = nEmbd / nHead

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

main :: Effect Unit
main = launchAff_ do
  content <- readTextFile UTF8 "src/input.txt"
  seed <- liftEffect randomSeed
  let
    numSteps = 1000
    _generated = flip evalGen { newSeed: seed, size: 0 } do
      dataset <- initDataset content
      let vocabSize = length (buildVocab dataset) + 1
      params <- initParams 16 4 1 16 vocabSize
      let
        numParams = length $ flatten params
        initialState =
          { params
          , dataset
          , m: replicate numParams 0.0
          , v: replicate numParams 0.0
          , numSteps
          , learningRate: 0.01
          , beta1: 0.85
          , beta2: 0.99
          , epsAdam: 1e-8
          }
        finalState = foldl train initialState (range 0 (numSteps - 1))
        trainedParams = extract <$> finalState.params
      inference trainedParams dataset
  pure unit
