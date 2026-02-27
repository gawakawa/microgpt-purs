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

newtype KVCache a = KVCache (List { key :: Vec a, value :: Vec a })

derive instance Newtype (KVCache a) _

instance Semigroup (KVCache a) where
  append (KVCache c1) (KVCache c2) = KVCache $ c1 <> c2

instance Monoid (KVCache a) where
  mempty = KVCache Nil

softmax :: forall a. Differentiable a => Vec a -> Vec a
softmax logits = (_ / sum exps) <$> exps
  where
  maxVal = foldl max (fromNumber (-N.infinity)) logits
  exps = (exp <<< (_ - maxVal)) <$> logits

rmsnorm :: forall a. Differentiable a => Vec a -> Vec a
rmsnorm x = ((*) scale) <$> x
  where
  ms = dot x x / fromNumber (toNumber (length x))
  scale = pow (ms + eps) (-0.5)
  eps = fromNumber 1e-5

withResidual :: forall f a. Functor f => Differentiable a => (Vec a -> f (Vec a)) -> Vec a -> f (Vec a)
withResidual f x = map (_ + x) (f $ rmsnorm x)

embed :: forall a. Matrix a -> Int -> Vec a
embed (Vec rows) = unsafePartial unsafeIndex rows

headAttn :: forall a. Differentiable a => Int -> Int -> Vec a -> List { key :: Vec a, value :: Vec a } -> Vec a
headAttn h headDim q cache = headOut
  where
  hs = h * headDim
  qH = Vec $ Array.slice hs (hs + headDim) (unwrap q)
  keys = Vec <<< Array.slice hs (hs + headDim) <<< unwrap <<< _.key <$> cache
  values = Vec <<< Array.slice hs (hs + headDim) <<< unwrap <<< _.value <$> cache
  attnLogits = Vec <<< List.toUnfoldable $ (\k -> dot qH k / pow (fromNumber (toNumber headDim)) 0.5) <$> keys
  attnWeights = softmax attnLogits
  headOut = foldl (+) (Vec $ Array.replicate headDim zero) $ List.zipWith (\w v -> (_ * w) <$> v) (List.fromFoldable $ unwrap attnWeights) values

multiHeadAttn :: forall a. Differentiable a => LayerWeights a -> Int -> KVCache a -> Vec a -> KVCache a /\ Vec a
multiHeadAttn weights headDim cache x = cache' /\ x'
  where
  w = unwrap weights
  q = linear w.attnWq x
  k = linear w.attnWk x
  v = linear w.attnWv x
  cache' = KVCache $ { key: k, value: v } : unwrap cache
  nHead = length q / headDim
  xAttn = Vec $ Array.concatMap (unwrap <<< (\h -> headAttn h headDim q (unwrap cache'))) (Array.range 0 $ nHead - 1)
  x' = linear w.attnWo xAttn

mlp :: forall a. Differentiable a => LayerWeights a -> Vec a -> Vec a
mlp weights = linear w.mlpFc2 <<< map relu <<< linear w.mlpFc1
  where
  w = unwrap weights

gpt :: forall a. Differentiable a => StateDict a -> Int -> Array (KVCache a) -> TokenId -> PosId -> Vec a /\ Array (KVCache a)
gpt stateDict headDim caches tokId posId = logits /\ caches'
  where
  sd = unwrap stateDict
  x = embed sd.wte (unwrap tokId) + embed sd.wpe (unwrap posId)
  step (cs /\ v) (w /\ c) = lmap (Array.snoc cs) $ (withResidual (multiHeadAttn w headDim c) >=> withResidual (\y -> mempty /\ mlp w y)) v
  caches' /\ x' = foldl step ([] /\ x) (Array.zipWith (/\) sd.layers caches)
  logits = linear sd.lmHead x'
