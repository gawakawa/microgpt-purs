module Inference where

import Prelude

import Control.Monad.Gen.Class (class MonadGen, chooseFloat)
import Control.Monad.Loops (unfoldrM)
import Control.Monad.State (StateT, evalStateT)
import Control.Monad.State.Class (get, put)
import Control.Monad.Trans.Class (lift)
import Data.Array (findIndex)
import Data.Foldable (length)
import Data.List (List)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap)
import Data.String.CodeUnits (fromCharArray)
import Data.Traversable (mapAccumL)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Unfoldable (replicate)
import GPT (KVCache, PosId(..), gpt, softmax)
import Matrix (Vec(..))
import Params (StateDict(..))
import Tokenizer (Token(..), bos, decode)

-- | Sample a token index from probability distribution
sample :: forall m. MonadGen m => Vec Number -> m Int
sample (Vec probs) = pick <$> chooseFloat 0.0 1.0
  where
  cumsum = (mapAccumL (\s p -> { accum: s + p, value: s + p }) 0.0 probs).value
  pick r = fromMaybe (length probs - 1) $ findIndex (_ > r) cumsum

-- | Predict next token: gpt → softmax → sample
predictNextToken
  :: forall m
   . MonadGen m
  => StateDict Number
  -> Int
  -> Number
  -> Token
  -> Int
  -> StateT (List (KVCache Number)) m Token
predictNextToken params headDim temperature tok pos = do
  caches <- get
  let logits /\ caches' = gpt params headDim caches tok (PosId pos)
  put caches'
  let probs = softmax $ (_ / temperature) <$> logits
  lift $ Token <$> sample probs

-- | Generate text that resembles the given dataset using trained weights
inference :: forall m. MonadGen m => StateDict Number -> Array String -> m String
inference params dataset = fromCharArray <$> evalStateT (unfoldrM step (bos /\ 0)) (replicate (length sd.layers) mempty)
  where
  sd = unwrap params
  temperature = 0.5

  step :: Token /\ Int -> StateT (List (KVCache Number)) m (Maybe (Char /\ Token /\ Int))
  step (tok /\ pos) = do
    nextTok <- predictNextToken params sd.headDim temperature tok pos
    pure $ (_ /\ nextTok /\ (pos + 1)) <$> decode nextTok
