module GPT where

import Prelude

import Data.Array (unsafeIndex)
import Data.Array as Array
import Data.List (List(..), (:))
import Data.List as List
import Data.Bifunctor (lmap)
import Data.Foldable (foldl, length, sum)
import Data.Int (toNumber)
import Data.Newtype (class Newtype, unwrap)
import Data.Number as N
import Data.Tuple.Nested (type (/\), (/\))
import Partial.Unsafe (unsafePartial)
import ComputationGraph (class Differentiable, exp, fromNumber, pow, relu)
import Matrix (Matrix, Vec(..), dot, linear)
import Params (LayerWeights(..), StateDict(..))

newtype TokenId = TokenId Int
newtype PosId = PosId Int

derive instance Newtype TokenId _
derive instance Newtype PosId _

newtype KVCache a = KVCache
  { keys :: List (Vec a)
  , values :: List (Vec a)
  }

derive instance Newtype (KVCache a) _

instance Semigroup (KVCache a) where
  append (KVCache c1) (KVCache c2) =
    KVCache { keys: c1.keys <> c2.keys, values: c1.values <> c2.values }

instance Monoid (KVCache a) where
  mempty = KVCache { keys: Nil, values: Nil }

softmax :: forall a. Differentiable a => Vec a -> Vec a
softmax logits = (_ / sum exps) <$> exps
  where
  maxVal = foldl max (fromNumber (-N.infinity)) logits
  exps = (exp <<< (_ - maxVal)) <$> logits

rmsnorm :: forall a. Differentiable a => Vec a -> Vec a
rmsnorm x = ((*) scale) <$> x
  where
  ms = sum (square <$> x) / fromNumber (toNumber (length x))
  scale = pow (ms + fromNumber 1e-5) (-0.5)
  square = join mul

withResidual :: forall f a. Functor f => Differentiable a => (Vec a -> f (Vec a)) -> Vec a -> f (Vec a)
withResidual f x = map (_ + x) (f $ rmsnorm x)

embed :: forall a. Matrix a -> Int -> Vec a
embed (Vec rows) = unsafePartial unsafeIndex rows

headAttn :: forall a. Differentiable a => Int -> Int -> Vec a -> List (Vec a) -> List (Vec a) -> Vec a
headAttn h headDim q keys values = headOut
  where
  hs = h * headDim
  qH = Vec $ Array.slice hs (hs + headDim) (unwrap q)
  kH = Vec <<< Array.slice hs (hs + headDim) <<< unwrap <$> keys
  vH = Vec <<< Array.slice hs (hs + headDim) <<< unwrap <$> values
  attnLogits = Vec <<< List.toUnfoldable $ (\k -> dot qH k / pow (fromNumber (toNumber headDim)) 0.5) <$> kH
  attnWeights = softmax attnLogits
  headOut = foldl (+) (Vec $ Array.replicate headDim zero) $ List.zipWith (\w v -> (_ * w) <$> v) (List.fromFoldable $ unwrap attnWeights) vH

multiHeadAttn :: forall a. Differentiable a => LayerWeights a -> Int -> KVCache a -> Vec a -> KVCache a /\ Vec a
multiHeadAttn weights headDim cache x = cache' /\ x'
  where
  w = unwrap weights
  q = linear w.attnWq x
  k = linear w.attnWk x
  v = linear w.attnWv x
  cache' = KVCache { keys: k : (unwrap cache).keys, values: v : (unwrap cache).values }
  nHead = length q / headDim
  xAttn = Vec $ Array.concatMap (unwrap <<< (\h -> headAttn h headDim q (unwrap cache').keys (unwrap cache').values)) (Array.range 0 $ nHead - 1)
  x' = linear w.attnWo xAttn

mlp :: forall a. Differentiable a => LayerWeights a -> Vec a -> Vec a
mlp weights = linear (unwrap weights).mlpFc2 <<< map relu <<< linear (unwrap weights).mlpFc1

gpt :: forall a. Differentiable a => StateDict a -> Int -> Array (KVCache a) -> TokenId -> PosId -> Vec a /\ Array (KVCache a)
gpt stateDict headDim caches tokId posId = logits /\ caches'
  where
  sd = unwrap stateDict
  x = embed sd.wte (unwrap tokId) + embed sd.wpe (unwrap posId)
  step (cs /\ v) (w /\ c) = lmap (Array.snoc cs) $ (withResidual (multiHeadAttn w headDim c) >=> withResidual (\y -> mempty /\ mlp w y)) v
  caches' /\ x' = foldl step ([] /\ x) (Array.zipWith (/\) sd.layers caches)
  logits = linear sd.lmHead x'
