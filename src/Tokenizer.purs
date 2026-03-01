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

buildVocab :: Array String -> Array Char
buildVocab = sort <<< nub <<< concatMap toCharArray

bos :: Token
bos = Token $ on (-) toCharCode 'z' 'a' + 1

encode :: Char -> Token
encode c = Token $ on (-) toCharCode c 'a'

decode :: Token -> Maybe Char
decode bos = Nothing
decode (Token tok) = fromCharCode $ tok + toCharCode 'a'

tokenize :: Array String -> Array Token
tokenize docs = surroundMap [ bos ] (map encode <<< toCharArray) docs
