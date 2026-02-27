module Matrix where

import Prelude

import Data.Array (zipWith)
import Data.Foldable (class Foldable, sum)
import Data.Newtype (class Newtype)
import Data.Traversable (class Traversable)

newtype Vec a = Vec (Array a)
type Matrix a = Vec (Vec a)

derive instance Newtype (Vec a) _
derive instance Eq a => Eq (Vec a)
derive newtype instance Show a => Show (Vec a)
derive newtype instance Functor Vec
derive newtype instance Foldable Vec
derive newtype instance Traversable Vec

instance Semiring a => Semiring (Vec a) where
  add (Vec u) (Vec v) = Vec (zipWith add u v)
  zero = Vec []
  mul (Vec u) (Vec v) = Vec (zipWith mul u v)
  one = Vec []

dot :: forall a. Semiring a => Vec a -> Vec a -> a
dot u v = sum $ u * v

linear :: forall a. Semiring a => Matrix a -> Vec a -> Vec a
linear (Vec w) x = Vec $ (\row -> dot row x) <$> w
