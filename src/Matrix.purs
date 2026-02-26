module Matrix where

import Prelude

import Data.Array (zipWith)
import Data.Foldable (sum)

type Matrix a = Array (Array a)

dot :: forall a. Semiring a => Array a -> Array a -> a
dot u v = sum $ zipWith (*) u v

linear :: forall a. Semiring a => Matrix a -> Array a -> Array a
linear w x = (\row -> dot row x) <$> w
