module ComputationGraph where

import Prelude

import Control.Comonad (class Comonad, class Extend, extend, extract)
import Control.Monad.State (State, evalState, gets, modify_)
import Data.Array as Array
import Data.Foldable (class Foldable, foldl, foldMap, foldr)
import Data.List (List(..), (:))
import Data.Graph.Weighted (fromEdges)
import Data.Graph.Weighted.DAG (DAG, unsafeFromWeightedDigraph)
import Data.Hashable (class Hashable, hash)
import Data.HashMap (HashMap)
import Data.HashMap as HashMap
import Data.Int (toNumber)
import Data.Int.Bits ((.^.))
import Data.Newtype (class Newtype, unwrap)
import Data.Number as N

class (Ord a, DivisionRing a, EuclideanRing a) <= Differentiable a where
  exp :: a -> a
  log :: a -> a
  pow :: a -> Number -> a
  sqrt :: a -> a
  relu :: a -> a
  fromNumber :: Number -> a
  fromInt :: Int -> a

instance Differentiable Number where
  exp = N.exp
  log = N.log
  pow = N.pow
  sqrt = N.sqrt
  relu = max 0.0
  fromNumber = identity
  fromInt = toNumber

newtype Hash = Hash Int

derive instance Newtype Hash _
derive newtype instance Eq Hash
derive newtype instance Ord Hash

instance Semigroup Hash where
  append (Hash a) (Hash b) = Hash $ (a * 16777619) .^. b

data ComputationGraph a
  = Val Hash a
  | Add Hash a (ComputationGraph a) (ComputationGraph a)
  | Mul Hash a (ComputationGraph a) (ComputationGraph a)
  | Pow Hash a (ComputationGraph a) Number
  | Exp Hash a (ComputationGraph a)
  | Log Hash a (ComputationGraph a)
  | Relu Hash a (ComputationGraph a)

nodeHash :: forall a. ComputationGraph a -> Hash
nodeHash (Val h _) = h
nodeHash (Add h _ _ _) = h
nodeHash (Mul h _ _ _) = h
nodeHash (Pow h _ _ _) = h
nodeHash (Exp h _ _) = h
nodeHash (Log h _ _) = h
nodeHash (Relu h _ _) = h

mkVal :: Number -> ComputationGraph Number
mkVal v = Val (Hash $ hash v) v

mkAdd :: ComputationGraph Number -> ComputationGraph Number -> ComputationGraph Number
mkAdd a b = Add (Hash 1 <> nodeHash a <> nodeHash b) (extract a + extract b) a b

mkMul :: ComputationGraph Number -> ComputationGraph Number -> ComputationGraph Number
mkMul a b = Mul (Hash 2 <> nodeHash a <> nodeHash b) (extract a * extract b) a b

mkPow :: ComputationGraph Number -> Number -> ComputationGraph Number
mkPow a n = Pow (Hash 3 <> nodeHash a <> Hash (hash n)) (N.pow (extract a) n) a n

mkExp :: ComputationGraph Number -> ComputationGraph Number
mkExp a = Exp (Hash 4 <> nodeHash a) (N.exp $ extract a) a

mkLog :: ComputationGraph Number -> ComputationGraph Number
mkLog a = Log (Hash 5 <> nodeHash a) (N.log $ extract a) a

mkRelu :: ComputationGraph Number -> ComputationGraph Number
mkRelu a = Relu (Hash 6 <> nodeHash a) (max 0.0 $ extract a) a

structuralEq :: forall a. Eq a => ComputationGraph a -> ComputationGraph a -> Boolean
structuralEq (Val _ v1) (Val _ v2) = v1 == v2
structuralEq (Add _ v1 a1 b1) (Add _ v2 a2 b2) = v1 == v2 && structuralEq a1 a2 && structuralEq b1 b2
structuralEq (Mul _ v1 a1 b1) (Mul _ v2 a2 b2) = v1 == v2 && structuralEq a1 a2 && structuralEq b1 b2
structuralEq (Pow _ v1 a1 n1) (Pow _ v2 a2 n2) = v1 == v2 && n1 == n2 && structuralEq a1 a2
structuralEq (Exp _ v1 a1) (Exp _ v2 a2) = v1 == v2 && structuralEq a1 a2
structuralEq (Log _ v1 a1) (Log _ v2 a2) = v1 == v2 && structuralEq a1 a2
structuralEq (Relu _ v1 a1) (Relu _ v2 a2) = v1 == v2 && structuralEq a1 a2
structuralEq _ _ = false

-- NOTE: Temporarily using hash-only comparison to test structuralEq bottleneck hypothesis.
-- This risks treating hash-colliding nodes as equal, but is acceptable for testing.
instance Eq a => Eq (ComputationGraph a) where
  eq a b = nodeHash a == nodeHash b

-- | NOTE: This Ord instance compares by extracted value (forward pass result),
-- | NOT by structure. This violates the Ord/Eq consistency law (a == b ⟺ compare a b == EQ)
-- | since Eq is structure-based for HashMap key usage.
-- | This is intentional: Ord is used for numerical operations like `max` in softmax,
-- | which require value-based comparison for numerical stability.
instance Ord a => Ord (ComputationGraph a) where
  compare a b = compare (extract a) (extract b)

instance Eq a => Hashable (ComputationGraph a) where
  hash = unwrap <<< nodeHash

derive instance Functor ComputationGraph

instance Semiring (ComputationGraph Number) where
  zero = mkVal 0.0
  one = mkVal 1.0
  add = mkAdd
  mul = mkMul

instance Ring (ComputationGraph Number) where
  sub a b = add a (mul (mkVal (-1.0)) b)

instance CommutativeRing (ComputationGraph Number)

instance DivisionRing (ComputationGraph Number) where
  recip a = mkPow a (-1.0)

instance EuclideanRing (ComputationGraph Number) where
  degree _ = 1
  div a b = mul a (recip b)
  mod _ _ = zero

instance Differentiable (ComputationGraph Number) where
  exp = mkExp
  log = mkLog
  pow = mkPow
  sqrt a = pow a 0.5
  relu = mkRelu
  fromNumber = mkVal
  fromInt = mkVal <<< toNumber

instance Show a => Show (ComputationGraph a) where
  show (Val _ v) = "(Val " <> show v <> ")"
  show (Add _ v a b) = "(Add " <> show v <> " " <> show a <> " " <> show b <> ")"
  show (Mul _ v a b) = "(Mul " <> show v <> " " <> show a <> " " <> show b <> ")"
  show (Pow _ v a n) = "(Pow " <> show v <> " " <> show a <> " " <> show n <> ")"
  show (Exp _ v a) = "(Exp " <> show v <> " " <> show a <> ")"
  show (Log _ v a) = "(Log " <> show v <> " " <> show a <> ")"
  show (Relu _ v a) = "(Relu " <> show v <> " " <> show a <> ")"

instance Extend ComputationGraph where
  extend f expr@(Val h _) = Val h (f expr)
  extend f expr@(Add h _ a b) = Add h (f expr) (extend f a) (extend f b)
  extend f expr@(Mul h _ a b) = Mul h (f expr) (extend f a) (extend f b)
  extend f expr@(Pow h _ a n) = Pow h (f expr) (extend f a) n
  extend f expr@(Exp h _ a) = Exp h (f expr) (extend f a)
  extend f expr@(Log h _ a) = Log h (f expr) (extend f a)
  extend f expr@(Relu h _ a) = Relu h (f expr) (extend f a)

instance Comonad ComputationGraph where
  extract (Val _ v) = v
  extract (Add _ v _ _) = v
  extract (Mul _ v _ _) = v
  extract (Pow _ v _ _) = v
  extract (Exp _ v _) = v
  extract (Log _ v _) = v
  extract (Relu _ v _) = v

instance Foldable ComputationGraph where
  foldMap f (Val _ v) = f v
  foldMap f (Add _ v a b) = f v <> foldMap f a <> foldMap f b
  foldMap f (Mul _ v a b) = f v <> foldMap f a <> foldMap f b
  foldMap f (Pow _ v a _) = f v <> foldMap f a
  foldMap f (Exp _ v a) = f v <> foldMap f a
  foldMap f (Log _ v a) = f v <> foldMap f a
  foldMap f (Relu _ v a) = f v <> foldMap f a
  foldl fn z cg = foldl fn z (foldMap pure cg :: Array _)
  foldr fn z cg = foldr fn z (foldMap pure cg :: Array _)

-- | Check if the node is a leaf (Val).
isVal :: forall a. ComputationGraph a -> Boolean
isVal (Val _ _) = true
isVal _ = false

type Edge = { source :: ComputationGraph Number, target :: ComputationGraph Number, weight :: Unit }
type Visited = HashMap (ComputationGraph Number) Unit

buildDag :: ComputationGraph Number -> DAG (ComputationGraph Number) Unit
buildDag root = unsafeFromWeightedDigraph $ fromEdges $ collectEdges root

collectEdges :: ComputationGraph Number -> Array Edge
collectEdges root = Array.fromFoldable $ evalState (go root Nil) HashMap.empty
  where
  go :: ComputationGraph Number -> List Edge -> State Visited (List Edge)
  go expr acc = gets (HashMap.member expr) >>= if _
    then pure acc
    else do
      modify_ $ HashMap.insert expr unit
      case expr of
        Val _ _ -> pure acc
        Add _ _ a b ->
          go a (edgeTo a : edgeTo b : acc) >>= go b
        Mul _ _ a b ->
          go a (edgeTo a : edgeTo b : acc) >>= go b
        Pow _ _ a _ ->
          go a (edgeTo a : acc)
        Exp _ _ a ->
          go a (edgeTo a : acc)
        Log _ _ a ->
          go a (edgeTo a : acc)
        Relu _ _ a ->
          go a (edgeTo a : acc)
    where
    edgeTo target = { source: expr, target, weight: unit }
