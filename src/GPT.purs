module GPT where

import Prelude

import Control.Monad.State (State, runState)
import Control.Monad.State.Class (get, modify_)
import Data.Array (range, unsafeIndex)
import Data.List (List(..), (:))
import Data.List as List
import Data.Foldable (fold, foldl, length, sum)
import Data.Newtype (class Newtype, unwrap)
import Data.Number as N
import Data.Bifunctor (lmap, rmap)
import Data.Tuple (uncurry)
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
type Hidden a = Vec a

type KVCache a = List { key :: Key a, value :: Value a }

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

-- | Compute combined token and position embeddings
tokenPosEmbedding :: forall a. Differentiable a => Matrix a -> Matrix a -> TokenId -> PosId -> Hidden a
tokenPosEmbedding wte wpe tokId posId = embed wte (unwrap tokId) + embed wpe (unwrap posId)

-- | Compute scaled dot-product attention.
attention :: forall a. Differentiable a => Query a -> Vec (Key a) -> Vec (Value a) -> Value a
attention query keys = weightedSum $ softmax $ (_ * scale) <$> linear keys query
  where
  scale = recip $ sqrt $ fromInt $ length query

-- | Extract Q/K/V slices for the h-th head.
headSlices :: forall a. Int -> Query a -> KVCache a -> Int -> Query a /\ Vec (Key a) /\ Vec (Value a)
headSlices headDim q cache h =
  sliceHead q /\ fromFoldable (sliceHead <<< _.key <$> cache) /\ fromFoldable (sliceHead <<< _.value <$> cache)
  where
  sliceHead = slice (h * headDim) headDim

-- | Compute multi-head attention with KV caching.
multiHeadAttn :: forall a. Differentiable a => LayerWeights a -> Int -> Vec a -> State (KVCache a) (Vec a)
multiHeadAttn weights headDim x = do
  modify_ \cache -> { key: k, value: v } : cache
  kvCache <- get
  pure $ linear w.attnWo $ fold $ uncurry (uncurry <<< attention) <<< headSlices headDim q kvCache <$> heads
  where
  w = unwrap weights
  q = linear w.attnWq x
  k = linear w.attnWk x
  v = linear w.attnWv x
  nHead = length q / headDim
  heads = range 0 $ nHead - 1

mlp :: forall a. Differentiable a => LayerWeights a -> Vec a -> Vec a
mlp weights = linear w.mlpFc2 <<< map relu <<< linear w.mlpFc1
  where
  w = unwrap weights

-- | Apply one transformer block: attention and MLP with residual connections
transformerBlock :: forall a. Differentiable a => LayerWeights a -> Int -> Hidden a -> State (KVCache a) (Hidden a)
transformerBlock weights headDim = withResidual (multiHeadAttn weights headDim) >=> withResidual (pure <<< mlp weights)

-- | Process input through all transformer layers
processLayers :: forall a. Differentiable a
  => Int -> List (LayerWeights a) -> List (KVCache a) -> Vec a
  -> Hidden a /\ List (KVCache a)
processLayers headDim layers caches input = foldl step (input /\ Nil) $ List.zipWith (/\) layers caches
  where
  step :: (Hidden a /\ List (KVCache a)) -> (LayerWeights a /\ KVCache a) -> Hidden a /\ List (KVCache a)
  step (hidden /\ accCaches) (weights /\ cache) =
    rmap (_ : accCaches) $ runState (transformerBlock weights headDim hidden) cache

-- | Compute next-token logits with updated KV caches for autoregressive generation
gpt :: forall a. Differentiable a => StateDict a -> Int -> List (KVCache a) -> TokenId -> PosId -> Vec a /\ List (KVCache a)
gpt stateDict headDim caches tokId posId =
  lmap (linear sd.lmHead) $ processLayers headDim sd.layers caches $ tokenPosEmbedding sd.wte sd.wpe tokId posId
  where
  sd = unwrap stateDict
