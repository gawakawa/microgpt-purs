module Main where

import Prelude

import Control.Comonad (class Comonad, class Extend, extend, extract)
import Control.Monad.Gen.Trans (evalGen, shuffle)
import Data.Array (concatMap, filter, length, nub)
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
import Random.LCG (Seed, randomSeed)

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

tokenize :: Array String -> Array Int
tokenize docs = surroundMap [ bos ] (map encode <<< toCharArray) docs
  where
  bos :: Int
  bos = length $ nub $ concatMap toCharArray docs

initDataset :: Seed -> String -> Array String
initDataset seed content = evalGen (shuffle docs) { newSeed: seed, size: 0 }
  where
  docs :: Array String
  docs = filter (not <<< null) $ trim <$> split (Pattern "\n") content

main :: Effect Unit
main = launchAff_ do
  content <- readTextFile UTF8 "src/input.txt"
  seed <- liftEffect randomSeed
  let _ = initDataset seed content
  pure unit
