module Main where

import Prelude

import Control.Monad.Gen.Trans (evalGenT, shuffle)
import Data.Array (filter, length)
import Data.String (Pattern(..), null, split, trim)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Random.LCG (randomSeed)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)

main :: Effect Unit
main = launchAff_ do
  content <- readTextFile UTF8 "src/input.txt"
  let docs = filter (not <<< null) $ trim <$> split (Pattern "\n") content
  seed <- liftEffect randomSeed
  shuffled <- liftEffect $ evalGenT (shuffle docs) { newSeed: seed, size: 0 }
  liftEffect $ log $ "num docs: " <> show (length shuffled)
