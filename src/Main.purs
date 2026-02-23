module Main where

import Prelude

import Control.Comonad (class Comonad, class Extend, extend, extract)
import Control.Monad.Gen.Class (class MonadGen, chooseFloat)
import Control.Monad.Gen.Trans (Gen, evalGen, shuffle)
import Data.Array (concatMap, filter, length, nub, sort)
import Data.Unfoldable (replicateA)
import Data.Char (toCharCode)
import Data.Foldable (foldl, surroundMap)
import Data.Function (on)
import Data.Graph.Weighted.DAG (DAG, topologicalSort)
import Data.Map (Map, fromFoldableWith, lookup, singleton, unionWith)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Number as N
import Data.String (Pattern(..), null, split, trim)
import Data.String.CodeUnits (toCharArray)
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import Random.LCG (randomSeed)

data Expr a
  = Val a
  | Add a (Expr a) (Expr a)
  | Mul a (Expr a) (Expr a)
  | Pow a (Expr a) Number
  | Exp a (Expr a)
  | Log a (Expr a)
  | Relu a (Expr a)

derive instance Eq a => Eq (Expr a)
derive instance Ord a => Ord (Expr a)
derive instance Functor Expr

instance Show a => Show (Expr a) where
  show (Val v) = "(Val " <> show v <> ")"
  show (Add v a b) = "(Add " <> show v <> " " <> show a <> " " <> show b <> ")"
  show (Mul v a b) = "(Mul " <> show v <> " " <> show a <> " " <> show b <> ")"
  show (Pow v a n) = "(Pow " <> show v <> " " <> show a <> " " <> show n <> ")"
  show (Exp v a) = "(Exp " <> show v <> " " <> show a <> ")"
  show (Log v a) = "(Log " <> show v <> " " <> show a <> ")"
  show (Relu v a) = "(Relu " <> show v <> " " <> show a <> ")"

instance Extend Expr where
  extend f expr@(Val _) = Val (f expr)
  extend f expr@(Add _ a b) = Add (f expr) (extend f a) (extend f b)
  extend f expr@(Mul _ a b) = Mul (f expr) (extend f a) (extend f b)
  extend f expr@(Pow _ a n) = Pow (f expr) (extend f a) n
  extend f expr@(Exp _ a) = Exp (f expr) (extend f a)
  extend f expr@(Log _ a) = Log (f expr) (extend f a)
  extend f expr@(Relu _ a) = Relu (f expr) (extend f a)

instance Comonad Expr where
  extract (Val v) = v
  extract (Add v _ _) = v
  extract (Mul v _ _) = v
  extract (Pow v _ _) = v
  extract (Exp v _) = v
  extract (Log v _) = v
  extract (Relu v _) = v

type GradMap = Map (Expr Number) Number

propagate :: Number -> Expr Number -> Array (Expr Number /\ Number)
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

backward :: Expr Number -> DAG (Expr Number) Unit -> GradMap
backward root dag = foldl step (singleton root 1.0) (topologicalSort dag)
  where
  step :: GradMap -> Expr Number -> GradMap
  step grads expr = fromMaybe grads do
    g <- lookup expr grads
    pure $ unionWith (+) grads (fromFoldableWith (+) $ propagate g expr)

encode :: Char -> Int
encode c = on (-) toCharCode c 'a'

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

matrix :: Int -> Int -> Gen (Matrix (Expr Number))
matrix nout nin = replicateA nout $ replicateA nin do
  g <- sampleGauss std
  pure $ Val g
  where
  std = 0.08

type Matrix a = Array (Array a)

type LayerWeights =
  { attnWq :: Matrix (Expr Number)
  , attnWk :: Matrix (Expr Number)
  , attnWv :: Matrix (Expr Number)
  , attnWo :: Matrix (Expr Number)
  , mlpFc1 :: Matrix (Expr Number)
  , mlpFc2 :: Matrix (Expr Number)
  }

type StateDict =
  { wte :: Matrix (Expr Number)
  , wpe :: Matrix (Expr Number)
  , lmHead :: Matrix (Expr Number)
  , layers :: Array LayerWeights
  }

initParams :: Int -> Int -> Int -> Int -> Int -> Gen StateDict
initParams nEmbd nHead nLayer blockSize vocabSize = do
  wte <- matrix vocabSize nEmbd
  wpe <- matrix blockSize nEmbd
  lmHead <- matrix vocabSize nEmbd
  layers <- replicateA nLayer do
    attnWq <- matrix nEmbd nEmbd
    attnWk <- matrix nEmbd nEmbd
    attnWv <- matrix nEmbd nEmbd
    attnWo <- matrix nEmbd nEmbd
    mlpFc1 <- matrix nEmbd nEmbd
    mlpFc2 <- matrix nEmbd nEmbd
    pure { attnWq, attnWk, attnWv, attnWo, mlpFc1, mlpFc2 }
  pure { wte, wpe, lmHead, layers }

flatten :: StateDict -> Array (Expr Number)
flatten sd = join (join <$> matrices)
  where
  matrices = [ sd.wte, sd.wpe, sd.lmHead ]
    <> concatMap layerMatrices sd.layers
  layerMatrices l = [ l.attnWq, l.attnWk, l.attnWv, l.attnWo, l.mlpFc1, l.mlpFc2 ]

main :: Effect Unit
main = launchAff_ do
  content <- readTextFile UTF8 "src/input.txt"
  seed <- liftEffect randomSeed
  let
    (dataset /\ params) = flip evalGen { newSeed: seed, size: 0 } do
      dataset <- initDataset content
      let vocabSize = length (buildVocab dataset) + 1
      params <- initParams 16 4 1 16 vocabSize
      pure $ dataset /\ params
  pure unit
