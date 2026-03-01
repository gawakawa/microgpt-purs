module Tokenizer where

import Prelude

import Data.Array (concatMap, nub, sort)
import Data.Char (fromCharCode, toCharCode)
import Data.Foldable (surroundMap)
import Data.Function (on)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.String.CodeUnits (toCharArray)

newtype Token = Token Int

derive instance Newtype Token _
derive instance Eq Token
derive newtype instance Show Token

newtype Pos = Pos Int

derive instance Newtype Pos _

-- | Build sorted unique character array from strings
buildVocab :: Array String -> Array Char
buildVocab = sort <<< nub <<< concatMap toCharArray

-- | Beginning of Sequence token (word boundary marker)
bos :: Token
bos = Token $ on (-) toCharCode 'z' 'a' + 1

-- | Convert character to token ('a' → 0, 'b' → 1, ...)
encode :: Char -> Token
encode c = Token $ on (-) toCharCode c 'a'

-- | Convert token back to character (Nothing for BOS)
decode :: Token -> Maybe Char
decode tok@(Token t)
  | tok == bos = Nothing
  | otherwise = fromCharCode $ t + toCharCode 'a'

-- | Convert strings to token sequence, surrounding each with BOS
tokenize :: Array String -> Array Token
tokenize docs = surroundMap [ bos ] (map encode <<< toCharArray) docs
