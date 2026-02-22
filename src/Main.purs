module Main where

import Prelude

import Control.Comonad (class Comonad, extract)
import Control.Extend (class Extend, extend)
import Control.Monad.Gen.Trans (evalGen, shuffle)
import Data.Array (concatMap, filter, length, nub)
import Data.Char (toCharCode)
import Data.Foldable (surroundMap)
import Data.Function (on)
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

derive instance Functor Tree
derive instance Eq a => Eq (Tree a)

instance Show a => Show (Tree a) where
  show (Leaf v) = "(Leaf " <> show v <> ")"
  show (Add v l r) = "(Add " <> show v <> " " <> show l <> " " <> show r <> ")"
  show (Mul v l r) = "(Mul " <> show v <> " " <> show l <> " " <> show r <> ")"

instance Extend Tree where
  extend f t@(Leaf _) = Leaf (f t)
  extend f t@(Add _ l r) = Add (f t) (extend f l) (extend f r)
  extend f t@(Mul _ l r) = Mul (f t) (extend f l) (extend f r)

instance Comonad Tree where
  extract (Leaf v) = v
  extract (Add v _ _) = v
  extract (Mul v _ _) = v

backward :: Tree Number -> Tree { val :: Number, grad :: Number }
backward = go 1.0
  where
  go :: Number -> Tree Number -> Tree { val :: Number, grad :: Number }
  go grad (Leaf v) =
    Leaf { val: v, grad }
  go grad (Add v left right) =
    Add { val: v, grad } (go grad left) (go grad right)
  go grad (Mul v left right) =
    Mul { val: v, grad } (go (grad * extract right) left) (go (grad * extract left) right)

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
