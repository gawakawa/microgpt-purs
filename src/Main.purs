module Main where

import Prelude

import Control.Comonad (class Comonad, class Extend, extend, extract)
import Control.Monad.Gen.Class (class MonadGen, chooseFloat)
import Control.Monad.Gen.Trans (Gen, evalGen, shuffle)
import Data.Array (concatMap, drop, filter, findIndex, fromFoldable, length, nub, range, replicate, slice, snoc, sort, unsafeIndex, zipWith)
import Data.Bifunctor (lmap)
import Data.Int (toNumber)
import Data.Char (toCharCode)
import Data.Foldable (class Foldable, foldl, foldMap, foldr, sum, surroundMap)
import Data.FoldableWithIndex (foldlWithIndex)
import Data.Traversable (class Traversable, traverse, mapAccumL)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Function (on)
import Data.Graph.Weighted (fromEdges)
import Data.Graph.Weighted.DAG (DAG, topologicalSort, unsafeFromWeightedDigraph)
import Data.Map (Map, fromFoldableWith, lookup, singleton, unionWith)
import Data.Maybe (Maybe(..), fromJust, fromMaybe)
import Data.Number as N
import Data.String (Pattern(..), null, split, trim)
import Data.String.CodeUnits (fromCharArray, toCharArray)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Unfoldable (replicateA)
import Data.Newtype (class Newtype, unwrap)
import Partial.Unsafe (unsafePartial)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import Random.LCG (randomSeed)

class (Ord a, EuclideanRing a) <= Differentiable a where
  exp :: a -> a
  log :: a -> a
  pow :: a -> Number -> a
  relu :: a -> a
  fromNumber :: Number -> a

instance Differentiable Number where
  exp = N.exp
  log = N.log
  pow = N.pow
  relu = max 0.0
  fromNumber = identity

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

decode :: Array Char -> Int -> Char
decode = unsafePartial unsafeIndex

buildVocab :: Array String -> Array Char
buildVocab = sort <<< nub <<< concatMap toCharArray

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

sample :: forall m. MonadGen m => Array Number -> m Int
sample probs = pick <$> chooseFloat 0.0 1.0
  where
  cumsum = (mapAccumL (\s p -> { accum: s + p, value: s + p }) 0.0 probs).value
  pick r = fromMaybe (length probs - 1) $ findIndex (_ > r) cumsum

matrix :: Int -> Int -> Gen (Matrix (ComputationGraph Number))
matrix nout nin = replicateA nout $ replicateA nin do
  g <- sampleGauss std
  pure $ Val g
  where
  std = 0.08

type Matrix a = Array (Array a)

newtype LayerWeights a = LayerWeights
  { attnWq :: Matrix a
  , attnWk :: Matrix a
  , attnWv :: Matrix a
  , attnWo :: Matrix a
  , mlpFc1 :: Matrix a
  , mlpFc2 :: Matrix a
  }

derive instance Newtype (LayerWeights a) _

instance Functor LayerWeights where
  map f (LayerWeights l) = LayerWeights
    { attnWq: map (map f) l.attnWq
    , attnWk: map (map f) l.attnWk
    , attnWv: map (map f) l.attnWv
    , attnWo: map (map f) l.attnWo
    , mlpFc1: map (map f) l.mlpFc1
    , mlpFc2: map (map f) l.mlpFc2
    }

instance Foldable LayerWeights where
  foldMap f (LayerWeights l) =
    foldMap (foldMap f) l.attnWq <> foldMap (foldMap f) l.attnWk
      <> foldMap (foldMap f) l.attnWv
      <> foldMap (foldMap f) l.attnWo
      <> foldMap (foldMap f) l.mlpFc1
      <> foldMap (foldMap f) l.mlpFc2
  foldl f z lw = foldl f z (foldMap pure lw :: Array _)
  foldr f z lw = foldr f z (foldMap pure lw :: Array _)

instance Traversable LayerWeights where
  traverse f (LayerWeights l) = ado
    attnWq <- traverse (traverse f) l.attnWq
    attnWk <- traverse (traverse f) l.attnWk
    attnWv <- traverse (traverse f) l.attnWv
    attnWo <- traverse (traverse f) l.attnWo
    mlpFc1 <- traverse (traverse f) l.mlpFc1
    mlpFc2 <- traverse (traverse f) l.mlpFc2
    in LayerWeights { attnWq, attnWk, attnWv, attnWo, mlpFc1, mlpFc2 }
  sequence = traverse identity

newtype StateDict a = StateDict
  { wte :: Matrix a
  , wpe :: Matrix a
  , lmHead :: Matrix a
  , layers :: Array (LayerWeights a)
  , headDim :: Int
  }

derive instance Newtype (StateDict a) _

instance Functor StateDict where
  map f (StateDict s) = StateDict
    { wte: map (map f) s.wte
    , wpe: map (map f) s.wpe
    , lmHead: map (map f) s.lmHead
    , layers: map (map f) s.layers
    , headDim: s.headDim
    }

instance Foldable StateDict where
  foldMap f (StateDict s) =
    foldMap (foldMap f) s.wte <> foldMap (foldMap f) s.wpe
      <> foldMap (foldMap f) s.lmHead
      <> foldMap (foldMap f) s.layers
  foldl f z sd = foldl f z (foldMap pure sd :: Array _)
  foldr f z sd = foldr f z (foldMap pure sd :: Array _)

instance Traversable StateDict where
  traverse f (StateDict s) = ado
    wte <- traverse (traverse f) s.wte
    wpe <- traverse (traverse f) s.wpe
    lmHead <- traverse (traverse f) s.lmHead
    layers <- traverse (traverse f) s.layers
    in StateDict { wte, wpe, lmHead, layers, headDim: s.headDim }
  sequence = traverse identity

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

dot :: forall a. Semiring a => Array a -> Array a -> a
dot u v = sum $ zipWith (*) u v

linear :: forall a. Semiring a => Matrix a -> Array a -> Array a
linear w x = (\row -> dot row x) <$> w

softmax :: forall a. Differentiable a => Array a -> Array a
softmax logits = (_ / sum exps) <$> exps
  where
  maxVal = foldl max (fromNumber (-N.infinity)) logits
  exps = (exp <<< (_ - maxVal)) <$> logits

rmsnorm :: forall a. Differentiable a => Array a -> Array a
rmsnorm x = ((*) scale) <$> x
  where
  ms = sum (square <$> x) / fromNumber (toNumber (length x))
  scale = pow (ms + fromNumber 1e-5) (-0.5)
  square = join mul

withResidual :: forall f a. Functor f => Differentiable a => (Array a -> f (Array a)) -> Array a -> f (Array a)
withResidual f x = map (zipWith (+) x) (f $ rmsnorm x)

newtype TokenId = TokenId Int
newtype PosId = PosId Int

newtype KVCache a = KVCache
  { keys :: Array (Array a)
  , values :: Array (Array a)
  }

derive instance Newtype (KVCache a) _

instance Semigroup (KVCache a) where
  append (KVCache c1) (KVCache c2) =
    KVCache { keys: c1.keys <> c2.keys, values: c1.values <> c2.values }

instance Monoid (KVCache a) where
  mempty = KVCache { keys: [], values: [] }

embedding :: forall a. Semiring a => Matrix a -> Matrix a -> TokenId -> PosId -> Array a
embedding wte wpe (TokenId tokId) (PosId posId) = zipWith (+) tokEmb posEmb
  where
  tokEmb = unsafePartial $ unsafeIndex wte tokId
  posEmb = unsafePartial $ unsafeIndex wpe posId

headAttn :: forall a. Differentiable a => Int -> Int -> Array a -> Array (Array a) -> Array (Array a) -> Array a
headAttn h headDim q keys values = headOut
  where
  hs = h * headDim
  qH = slice hs (hs + headDim) q
  kH = slice hs (hs + headDim) <$> keys
  vH = slice hs (hs + headDim) <$> values
  attnLogits = (\k -> dot qH k / pow (fromNumber (toNumber headDim)) 0.5) <$> kH
  attnWeights = softmax attnLogits
  headOut = foldl (zipWith (+)) (replicate headDim zero) (zipWith (\w v -> (_ * w) <$> v) attnWeights vH)

multiHeadAttn :: forall a. Differentiable a => LayerWeights a -> Int -> KVCache a -> Array a -> KVCache a /\ Array a
multiHeadAttn weights headDim cache x = cache' /\ x'
  where
  w = unwrap weights
  q = linear w.attnWq x
  k = linear w.attnWk x
  v = linear w.attnWv x
  cache' = KVCache { keys: snoc (unwrap cache).keys k, values: snoc (unwrap cache).values v }
  nHead = length q / headDim
  xAttn = concatMap (\h -> headAttn h headDim q (unwrap cache').keys (unwrap cache').values) (range 0 $ nHead - 1)
  x' = linear w.attnWo xAttn

mlp :: forall a. Differentiable a => LayerWeights a -> Array a -> Array a
mlp weights = linear w.mlpFc2 <<< map relu <<< linear w.mlpFc1
  where
  w = unwrap weights

gpt :: forall a. Differentiable a => StateDict a -> Int -> Array (KVCache a) -> TokenId -> PosId -> Array a /\ Array (KVCache a)
gpt stateDict headDim caches tokId posId = logits /\ caches'
  where
  sd = unwrap stateDict
  x = embedding sd.wte sd.wpe tokId posId
  step (cs /\ v) (w /\ c) = lmap (snoc cs) $ (withResidual (multiHeadAttn w headDim c) >=> withResidual (\y -> mempty /\ mlp w y)) v
  caches' /\ x' = foldl step ([] /\ x) (zipWith (/\) sd.layers caches)
  logits = linear sd.lmHead x'

inference :: forall m. MonadGen m => StateDict Number -> Array String -> m String
inference params dataset = generate (blockSize - 1) initialCaches [] (bos /\ 0)
  where
  sd = unwrap params
  vocab = buildVocab dataset
  bos = length vocab
  nLayer = length sd.layers
  initialCaches = replicate nLayer mempty
  blockSize = length sd.wpe
  temperature = 0.5

  generate :: Int -> Array (KVCache Number) -> Array Char -> (Int /\ Int) -> m String
  generate 0 _ chars _ = pure $ fromCharArray chars
  generate n caches chars (tok /\ pos) = do
    let logits /\ caches' = gpt params sd.headDim caches (TokenId tok) (PosId pos)
    let probs = softmax $ (_ / temperature) <$> logits
    nextTok <- sample probs
    if nextTok == bos then
      pure $ fromCharArray chars
    else do
      let char = decode vocab nextTok
      generate (n - 1) caches' (snoc chars char) (nextTok /\ (pos + 1))

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
