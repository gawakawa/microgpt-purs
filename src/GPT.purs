module GPT where

import Prelude

import Data.Array (concatMap, length, range, replicate, slice, snoc, zipWith)
import Data.Bifunctor (lmap)
import Data.Foldable (foldl, sum)
import Data.Int (toNumber)
import Data.Newtype (class Newtype, unwrap)
import Data.Number as N
import Data.Tuple.Nested (type (/\), (/\))
import Partial.Unsafe (unsafePartial)
import Data.Array.Partial (unsafeIndex)
import ComputationGraph (class Differentiable, exp, fromNumber, pow, relu)
import Matrix (Matrix, dot, linear)
import Params (LayerWeights(..), StateDict(..))

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
