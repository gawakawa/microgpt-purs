module Autograd where

import Prelude

import Control.Comonad (class Comonad, class Extend, extend, extract)
import Data.Foldable (class Foldable, foldl, foldMap, foldr)
import Data.Graph.Weighted (fromEdges)
import Data.Graph.Weighted.DAG (DAG, topologicalSort, unsafeFromWeightedDigraph)
import Data.Map (Map, fromFoldableWith, lookup, singleton, unionWith)
import Data.Maybe (fromMaybe)
import Data.Number as N
import Data.Tuple.Nested (type (/\), (/\))
import Inference (class Differentiable)

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

instance Foldable ComputationGraph where
  foldMap f (Val v) = f v
  foldMap f (Add v a b) = f v <> foldMap f a <> foldMap f b
  foldMap f (Mul v a b) = f v <> foldMap f a <> foldMap f b
  foldMap f (Pow v a _) = f v <> foldMap f a
  foldMap f (Exp v a) = f v <> foldMap f a
  foldMap f (Log v a) = f v <> foldMap f a
  foldMap f (Relu v a) = f v <> foldMap f a
  foldl fn z cg = foldl fn z (foldMap pure cg :: Array _)
  foldr fn z cg = foldr fn z (foldMap pure cg :: Array _)

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
