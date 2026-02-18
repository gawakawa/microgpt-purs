module Main where

import Prelude

import Data.Array (filter, length, take)
import Data.String (Pattern(..), null, split, trim)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Shuffle (shuffle)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)

main :: Effect Unit
main = launchAff_ do
  content <- readTextFile UTF8 "src/input.txt"
  -- avoid stack overflow
  let docs = take 100 $ filter (not <<< null) $ trim <$> split (Pattern "\n") content
  shuffled <- liftEffect $ shuffle docs
  liftEffect $ log $ "num docs: " <> show (length shuffled)
