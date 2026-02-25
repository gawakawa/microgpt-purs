module Inference where

import Prelude

import Control.Monad.Gen.Class (class MonadGen, chooseFloat)
import Data.Array (concatMap, findIndex, length, nub, range, replicate, slice, snoc, sort, zipWith)
import Data.Bifunctor (lmap)
import Data.Foldable (class Foldable, foldMap, foldl, foldr, sum)
import Data.Int (toNumber)
import Data.Maybe (fromMaybe)
import Data.Newtype (class Newtype, unwrap)
import Data.String.CodeUnits (fromCharArray, toCharArray)
import Data.Traversable (class Traversable, traverse, mapAccumL)
import Data.Tuple.Nested (type (/\), (/\))
import Partial.Unsafe (unsafePartial, unsafeIndex)

class (Ord a, EuclideanRing a) <= Differentiable a where
  exp :: a -> a
  log :: a -> a
  pow :: a -> Number -> a
  relu :: a -> a
  fromNumber :: Number -> a

import Data.Number as N

instance Differentiable Number where
  exp = N.exp
  log = N.log
  pow = N.pow
  relu = max 0.0
  fromNumber = identity

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

decode :: Array Char -> Int -> Char
decode = unsafePartial unsafeIndex

buildVocab :: Array String -> Array Char
buildVocab = sort <<< nub <<< concatMap toCharArray

sample :: forall m. MonadGen m => Array Number -> m Int
sample probs = pick <$> chooseFloat 0.0 1.0
  where
  cumsum = (mapAccumL (\s p -> { accum: s + p, value: s + p }) 0.0 probs).value
  pick r = fromMaybe (length probs - 1) $ findIndex (_ > r) cumsum

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
