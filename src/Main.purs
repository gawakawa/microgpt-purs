module Main where

import Prelude

import Control.Monad.Gen.Trans (evalGenT, shuffle)
import Data.Array (concatMap, filter, length, nub)
import Data.Foldable (surroundMap)
import Data.Char (toCharCode)
import Data.String (Pattern(..), null, split, trim)
import Data.String.CodeUnits (toCharArray)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Random.LCG (randomSeed)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)

encode :: Char -> Int
encode c = toCharCode c - toCharCode 'a'

tokenize :: Array String -> Array Int
tokenize docs = surroundMap [ bos ] (map encode <<< toCharArray) docs
  where
  bos = length $ nub $ concatMap toCharArray docs

main :: Effect Unit
main = launchAff_ do
  content <- readTextFile UTF8 "src/input.txt"
  let docs = filter (not <<< null) $ trim <$> split (Pattern "\n") content
  seed <- liftEffect randomSeed
  shuffled <- liftEffect $ evalGenT (shuffle docs) { newSeed: seed, size: 0 }
  pure unit
