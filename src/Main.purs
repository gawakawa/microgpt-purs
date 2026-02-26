module Main where

import Prelude

import Control.Comonad (extract)
import Control.Monad.Gen.Trans (evalGen)
import Data.Array (length, range, replicate)
import Data.Foldable (foldl)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import Random.LCG (randomSeed)
import Params (initParams)
import Inference (buildVocab, inference)
import Train (TrainState, flatten, initDataset, train)

main :: Effect Unit
main = launchAff_ do
  content <- readTextFile UTF8 "src/input.txt"
  seed <- liftEffect randomSeed
  let
    numSteps = 1000
    _generated = flip evalGen { newSeed: seed, size: 0 } do
      dataset <- initDataset content
      let vocabSize = length (buildVocab dataset) + 1
      params <- initParams 16 4 1 16 vocabSize
      let
        numParams = length $ flatten params

        initialState :: TrainState
        initialState =
          { params
          , dataset
          , m: replicate numParams 0.0
          , v: replicate numParams 0.0
          , numSteps
          , learningRate: 0.01
          , beta1: 0.85
          , beta2: 0.99
          , epsAdam: 1e-8
          }
        finalState = foldl train initialState (range 0 (numSteps - 1))
        trainedParams = extract <$> finalState.params
      inference trainedParams dataset
  pure unit
