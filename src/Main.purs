module Main where

import Prelude

import Control.Monad.Gen.Trans (evalGen, shuffle)
import Data.Array (concatMap, filter, length, nub)
import Data.Foldable (surroundMap)
import Data.Char (toCharCode)
import Data.String (Pattern(..), null, split, trim)
import Data.String.CodeUnits (toCharArray)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Random.LCG (Seed, randomSeed)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)

encode :: Char -> Int
encode c = toCharCode c - toCharCode 'a'

tokenize :: Array String -> Array Int
tokenize docs = surroundMap [ bos ] (map encode <<< toCharArray) docs
  where
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
