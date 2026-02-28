module GPT where

import Prelude

import Control.Monad.State (State, runState)
import Control.Monad.State.Class (gets, modify_)
import Data.Array (unsafeIndex)
import Data.Array as Array
import Data.Enum (enumFromTo)
import Data.List (List(..), (:))
import Data.List as List
import Data.Foldable (foldMap, foldl, length, sum)
import Data.Newtype (class Newtype, unwrap)
import Data.Number as N
import Data.Tuple.Nested (type (/\), (/\))
import Partial.Unsafe (unsafePartial)
import ComputationGraph (class Differentiable, exp, fromInt, fromNumber, pow, relu)
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
  ms = dot x x / fromInt (length x)
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
  attnLogits = Vec <<< List.toUnfoldable $ (\k -> dot qH k / pow (fromInt headDim) 0.5) <$> keys
  attnWeights = softmax attnLogits
  headOut = foldl (+) (Vec $ Array.replicate headDim zero) $ List.zipWith (\w v -> (_ * w) <$> v) (List.fromFoldable $ unwrap attnWeights) values

multiHeadAttn :: forall a. Differentiable a => LayerWeights a -> Int -> Vec a -> State (KVCache a) (Vec a)
multiHeadAttn weights headDim x = do
  modify_ \(KVCache list) -> KVCache $ { key: k, value: v } : list
  kvList <- gets unwrap
  pure $ linear w.attnWo $ foldMap (\h -> headAttn h headDim q kvList) heads
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
