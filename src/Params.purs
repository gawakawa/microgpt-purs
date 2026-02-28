module Params where

import Prelude

import Control.Monad.Gen.Class (class MonadGen, chooseFloat)
import Control.Monad.Gen.Trans (Gen)
import Data.Foldable (class Foldable, foldMap, foldl, foldr)
import Data.List (List)
import Data.Newtype (class Newtype)
import Data.Number as N
import Data.Traversable (class Traversable, traverse)
import Data.Unfoldable (replicateA)
import ComputationGraph (ComputationGraph(..))
import Matrix (Matrix, Vec(..))

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
  , layers :: List (LayerWeights a)
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

sampleGauss :: forall m. MonadGen m => Number -> m Number
sampleGauss std = do
  u1 <- chooseFloat 1.0e-7 1.0
  u2 <- chooseFloat 0.0 1.0
  let z = N.sqrt (-2.0 * N.log u1) * N.cos (2.0 * N.pi * u2)
  pure $ z * std

matrix :: Int -> Int -> Gen (Matrix (ComputationGraph Number))
matrix nout nin = Vec <$> replicateA nout do
  Vec <$> replicateA nin do
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
