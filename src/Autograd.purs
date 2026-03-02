module Autograd where

import Prelude

import Control.Comonad (extract)
import Data.Foldable (foldl)
import Data.Graph.Weighted.DAG (topologicalSort)
import Data.Map (Map, fromFoldableWith, lookup, singleton, unionWith)
import Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Number as N
import Data.Tuple.Nested (type (/\), (/\))
import ComputationGraph (ComputationGraph(..), buildDag, isVal)

type GradMap = Map (ComputationGraph Number) Number

propagate :: Number -> ComputationGraph Number -> Array (ComputationGraph Number /\ Number)
propagate g = case _ of
  Val _ _ -> []
  -- ∂(a+b)/∂a = 1, ∂(a+b)/∂b = 1
  Add _ _ a b -> [ a /\ g, b /\ g ]
  -- ∂(a·b)/∂a = b, ∂(a·b)/∂b = a
  Mul _ _ a b -> [ a /\ (g * extract b), b /\ (g * extract a) ]
  -- ∂aⁿ/∂a = n·aⁿ⁻¹
  Pow _ _ a n -> [ a /\ (g * n * N.pow (extract a) (n - 1.0)) ]
  -- ∂eᵃ/∂a = eᵃ
  Exp _ v a -> [ a /\ (g * v) ]
  -- ∂(ln a)/∂a = 1/a
  Log _ _ a -> [ a /\ (g / extract a) ]
  -- ∂max(0,a)/∂a = 1 if a>0, else 0
  Relu _ _ a -> [ a /\ (g * if extract a > 0.0 then 1.0 else 0.0) ]

-- | Compute gradients for leaf nodes (Val) via backpropagation.
-- | Returns a map from each leaf node to its gradient.
backward :: ComputationGraph Number -> GradMap
backward root = Map.filterKeys isVal $ foldl step (singleton root 1.0) $ topologicalSort $ buildDag root
  where
  step :: GradMap -> ComputationGraph Number -> GradMap
  step grads expr = fromMaybe grads do
    g <- lookup expr grads
    pure $ unionWith (+) grads (fromFoldableWith (+) $ propagate g expr)

