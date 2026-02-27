module Matrix where

import Prelude

import Data.Array (zipWith)
import Data.Foldable (sum)
import Data.Newtype (class Newtype)

type Matrix a = Array (Array a)

newtype Vec a = Vec (Array a)

derive instance Newtype (Vec a) _

instance Semiring a => Semiring (Vec a) where
  add (Vec u) (Vec v) = Vec (zipWith add u v)
  zero = Vec []
  mul (Vec u) (Vec v) = Vec (zipWith mul u v)
  one = Vec []

dot :: forall a. Semiring a => Array a -> Array a -> a
dot u v = sum $ zipWith (*) u v

linear :: forall a. Semiring a => Matrix a -> Array a -> Array a
linear w x = (\row -> dot row x) <$> w
