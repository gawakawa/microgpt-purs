module Main where

import Prelude

import Control.Comonad (class Comonad, extract)
import Control.Extend (class Extend, extend)
import Control.Monad.Gen.Trans (evalGen, shuffle)
import Data.Array (concatMap, filter, length, nub)
import Data.Char (toCharCode)
import Data.Foldable (surroundMap)
import Data.Function (on)
import Data.Number as N
import Data.String (Pattern(..), null, split, trim)
import Data.String.CodeUnits (toCharArray)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import Random.LCG (Seed, randomSeed)

data Tree a
  = Leaf a
  | Add a (Tree a) (Tree a)
  | Mul a (Tree a) (Tree a)
  | Pow a (Tree a) Number
  | Exp a (Tree a)
  | Log a (Tree a)
  | Relu a (Tree a)

derive instance Functor Tree
derive instance Eq a => Eq (Tree a)

instance Show a => Show (Tree a) where
  show (Leaf v) = "(Leaf " <> show v <> ")"
  show (Add v l r) = "(Add " <> show v <> " " <> show l <> " " <> show r <> ")"
  show (Mul v l r) = "(Mul " <> show v <> " " <> show l <> " " <> show r <> ")"
  show (Pow v child n) = "(Pow " <> show v <> " " <> show child <> " " <> show n <> ")"
  show (Exp v child) = "(Exp " <> show v <> " " <> show child <> ")"
  show (Log v child) = "(Log " <> show v <> " " <> show child <> ")"
  show (Relu v child) = "(Relu " <> show v <> " " <> show child <> ")"

instance Extend Tree where
  extend f t@(Leaf _) = Leaf (f t)
  extend f t@(Add _ l r) = Add (f t) (extend f l) (extend f r)
  extend f t@(Mul _ l r) = Mul (f t) (extend f l) (extend f r)
  extend f t@(Pow _ child n) = Pow (f t) (extend f child) n
  extend f t@(Exp _ child) = Exp (f t) (extend f child)
  extend f t@(Log _ child) = Log (f t) (extend f child)
  extend f t@(Relu _ child) = Relu (f t) (extend f child)

instance Comonad Tree where
  extract (Leaf v) = v
  extract (Add v _ _) = v
  extract (Mul v _ _) = v
  extract (Pow v _ _) = v
  extract (Exp v _) = v
  extract (Log v _) = v
  extract (Relu v _) = v

backward :: Tree Number -> Tree { val :: Number, grad :: Number }
backward = go 1.0
  where
  go :: Number -> Tree Number -> Tree { val :: Number, grad :: Number }
  go grad (Leaf v) =
    Leaf { val: v, grad }
  -- ∂(a+b)/∂a = 1, ∂(a+b)/∂b = 1
  go grad (Add v left right) =
    Add { val: v, grad } (go grad left) (go grad right)
  -- ∂(a·b)/∂a = b, ∂(a·b)/∂b = a
  go grad (Mul v left right) =
    Mul { val: v, grad } (go (grad * extract right) left) (go (grad * extract left) right)
  -- ∂aⁿ/∂a = n·aⁿ⁻¹
  go grad (Pow v child n) =
    Pow { val: v, grad } (go (grad * n * N.pow (extract child) (n - 1.0)) child) n
  -- ∂eᵃ/∂a = eᵃ
  go grad (Exp v child) =
    Exp { val: v, grad } (go (grad * v) child)
  -- ∂(ln a)/∂a = 1/a
  go grad (Log v child) =
    Log { val: v, grad } (go (grad / extract child) child)
  -- ∂max(0,a)/∂a = 1 if a>0, else 0
  go grad (Relu v child) =
    Relu { val: v, grad } (go (grad * if extract child > 0.0 then 1.0 else 0.0) child)

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
  let dataset = initDataset seed content
  pure unit
