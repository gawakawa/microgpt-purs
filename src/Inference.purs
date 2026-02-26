module Inference where

import Prelude

import Control.Monad.Gen.Class (class MonadGen, chooseFloat)
import Data.Array (concatMap, findIndex, length, nub, replicate, snoc, sort, unsafeIndex)
import Data.Maybe (fromMaybe)
import Data.Newtype (unwrap)
import Data.String.CodeUnits (fromCharArray, toCharArray)
import Data.Traversable (mapAccumL)
import Data.Tuple.Nested (type (/\), (/\))
import Partial.Unsafe (unsafePartial)
import GPT (KVCache, TokenId(..), PosId(..), gpt, softmax)
import Params (StateDict(..))

decode :: Array Char -> Int -> Char
decode = unsafePartial unsafeIndex

buildVocab :: Array String -> Array Char
buildVocab = sort <<< nub <<< concatMap toCharArray

sample :: forall m. MonadGen m => Array Number -> m Int
sample probs = pick <$> chooseFloat 0.0 1.0
  where
  cumsum = (mapAccumL (\s p -> { accum: s + p, value: s + p }) 0.0 probs).value
  pick r = fromMaybe (length probs - 1) $ findIndex (_ > r) cumsum

inference :: forall m. MonadGen m => StateDict Number -> Array String -> m String
inference params dataset = generate (blockSize - 1) initialCaches [] (bos /\ 0)
  where
  sd = unwrap params
  vocab = buildVocab dataset
  bos = length vocab
  nLayer = length sd.layers
  initialCaches = replicate nLayer mempty
  blockSize = length sd.wpe
  temperature = 0.5

  generate :: Int -> Array (KVCache Number) -> Array Char -> (Int /\ Int) -> m String
  generate 0 _ chars _ = pure $ fromCharArray chars
  generate n caches chars (tok /\ pos) = do
    let logits /\ caches' = gpt params sd.headDim caches (TokenId tok) (PosId pos)
    let probs = softmax $ (_ / temperature) <$> logits
    nextTok <- sample probs
    if nextTok == bos then
      pure $ fromCharArray chars
    else do
      let char = decode vocab nextTok
      generate (n - 1) caches' (snoc chars char) (nextTok /\ (pos + 1))
