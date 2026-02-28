module GPT where

import Prelude

import Control.Monad.State (State, runState)
import Control.Monad.State.Class (get, modify_)
import Data.Array (unsafeIndex)
import Data.Enum (enumFromTo)
import Data.List (List(..), (:))
import Data.List as List
import Data.Foldable (fold, foldl, length, sum)
import Data.Newtype (class Newtype, unwrap)
import Data.Number as N
import Data.Tuple.Nested (type (/\), (/\))
import Partial.Unsafe (unsafePartial)
import Data.DivisionRing (recip)
import ComputationGraph (class Differentiable, exp, fromInt, fromNumber, relu, sqrt)
import Matrix (Matrix, Vec(..), dot, fromFoldable, linear, slice, weightedSum)
import Params (LayerWeights(..), StateDict(..))

newtype TokenId = TokenId Int
newtype PosId = PosId Int

derive instance Newtype TokenId _
derive instance Newtype PosId _

type Query a = Vec a
type Key a = Vec a
type Value a = Vec a

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
rmsnorm x = (_ * scale) <$> x
  where
  scale = recip $ sqrt $ dot x x / fromInt (length x) + eps
  eps = fromNumber 1e-5

withResidual :: forall f a. Functor f => Differentiable a => (Vec a -> f (Vec a)) -> Vec a -> f (Vec a)
withResidual f x = map (_ + x) (f $ rmsnorm x)

embed :: forall a. Matrix a -> Int -> Vec a
embed (Vec rows) = unsafePartial unsafeIndex rows

-- | Compute scaled dot-product attention.
attention :: forall a. Differentiable a => Query a -> Vec (Key a) -> Vec (Value a) -> Value a
attention query keys = weightedSum $ softmax $ (_ * scale) <$> linear keys query
  where
  scale = recip $ sqrt $ fromInt $ length query

-- | Compute attention for the h-th head.
singleHeadAttn :: forall a. Differentiable a => Int -> Query a -> KVCache a -> Int -> Value a
singleHeadAttn headDim q (KVCache cache) h = attention qH keys values
  where
  qH = slice hs headDim q
  keys = fromFoldable $ slice hs headDim <<< _.key <$> cache
  values = fromFoldable $ slice hs headDim <<< _.value <$> cache
  hs = h * headDim

-- | Compute multi-head attention with KV caching.
multiHeadAttn :: forall a. Differentiable a => LayerWeights a -> Int -> Vec a -> State (KVCache a) (Vec a)
multiHeadAttn weights headDim x = do
  modify_ \(KVCache list) -> KVCache $ { key: k, value: v } : list
  kvCache <- get
  let headResults = singleHeadAttn headDim q kvCache <$> heads
  pure $ linear w.attnWo $ fold headResults
  where
  w = unwrap weights
  q = linear w.attnWq x
  k = linear w.attnWk x
  v = linear w.attnWv x
  nHead = length q / headDim
  heads :: Array Int
  heads = enumFromTo 0 $ nHead - 1

mlp :: forall a. Differentiable a => LayerWeights a -> Vec a -> Vec a
mlp weights = linear w.mlpFc2 <<< map relu <<< linear w.mlpFc1
  where
  w = unwrap weights

gpt :: forall a. Differentiable a => StateDict a -> Int -> List (KVCache a) -> TokenId -> PosId -> Vec a /\ List (KVCache a)
gpt stateDict headDim caches tokId posId = logits /\ caches'
  where
  sd = unwrap stateDict
  x = embed sd.wte (unwrap tokId) + embed sd.wpe (unwrap posId)
  step (cs /\ v) (w /\ c) = (c' : cs) /\ result
    where
    stateComp = withResidual (multiHeadAttn w headDim) >=> withResidual (pure <<< mlp w)
    result /\ c' = runState (stateComp v) c
  caches' /\ x' = foldl step (Nil /\ x) $ List.zipWith (/\) sd.layers caches
  logits = linear sd.lmHead x'
