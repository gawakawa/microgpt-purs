module Matrix where

import Prelude

import Data.Array (replicate, zipWith)
import Data.Array as Array
import Data.Foldable (class Foldable, foldl, length, sum)
import Data.Maybe (maybe)
import Data.Newtype (class Newtype, unwrap)
import Data.Traversable (class Traversable)

newtype Vec a = Vec (Array a)
type Matrix a = Vec (Vec a)

derive instance Newtype (Vec a) _
derive instance Eq a => Eq (Vec a)
derive newtype instance Show a => Show (Vec a)
derive newtype instance Functor Vec
derive newtype instance Foldable Vec
derive newtype instance Traversable Vec

instance Apply Vec where
  apply (Vec fs) (Vec xs) = Vec (zipWith ($) fs xs)
derive newtype instance Semigroup (Vec a)
derive newtype instance Monoid (Vec a)

instance Semiring a => Semiring (Vec a) where
  add (Vec u) (Vec v) = Vec (zipWith add u v)
  zero = Vec []
  mul (Vec u) (Vec v) = Vec (zipWith mul u v)
  one = Vec []

dot :: forall a. Semiring a => Vec a -> Vec a -> a
dot u v = sum $ u * v

linear :: forall a. Semiring a => Matrix a -> Vec a -> Vec a
linear (Vec w) x = Vec $ (\row -> dot row x) <$> w

zeroVec :: forall a. Semiring a => Int -> Vec a
zeroVec n = Vec $ replicate n zero

fromFoldable :: forall f a. Foldable f => f a -> Vec a
fromFoldable = Vec <<< Array.fromFoldable

slice :: forall a. Int -> Int -> Vec a -> Vec a
slice start len = Vec <<< Array.slice start (start + len) <<< unwrap

-- | Compute weighted sum: Σ wᵢ * vᵢ (linear combination of rows)
weightedSum :: forall a. Semiring a => Vec a -> Matrix a -> Vec a
weightedSum (Vec ws) (Vec rows) = foldl (+) (zeroVec dim) scaled
  where
  dim = maybe 0 length $ Array.head rows
  scaled = zipWith (\w row -> (_ * w) <$> row) ws rows
