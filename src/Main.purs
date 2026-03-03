module Main where

import Prelude

import Control.Comonad (extract)
import Control.Monad.Gen.Trans (GenT(..), evalGenT, shuffle)
import Control.Monad.State.Trans (lift)
import Data.Foldable (length)
import Data.String (Pattern(..), split, trim)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Console as Console
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import Random.LCG (randomSeed)
import Matrix (zeroVec)
import Params (initParams)
import Inference (inference)
import Tokenizer (buildVocab)
import Train (TrainState, flatten, initDataset, train)

main :: Effect Unit
main = launchAff_ do
  content <- readTextFile UTF8 "src/input.txt"
  seed <- liftEffect randomSeed
  let
    numSteps = 1000
    parsedDocs = trim <$> split (Pattern "\n") content
  _ <- flip evalGenT { newSeed: seed, size: 0 } do
    let log = GenT <<< lift <<< liftEffect <<< Console.log
    shuffledDocs <- shuffle parsedDocs
    dataset <- initDataset log shuffledDocs
    let vocabSize = length (buildVocab dataset) + 1
    log $ "vocab size: " <> show vocabSize
    params <- initParams 16 4 1 16 vocabSize
    let numParams = length $ flatten params
    log $ "num params: " <> show numParams
    let
      initialState :: TrainState
      initialState =
        { params
        , dataset
        , m: zeroVec numParams
        , v: zeroVec numParams
        , numSteps
        , learningRate: 0.01
        , beta1: 0.85
        , beta2: 0.99
        , epsAdam: 1e-8
        }
    finalState <- train log initialState
    let trainedParams = extract <$> finalState.params
    inference log trainedParams dataset
  pure unit
