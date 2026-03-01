module Tokenizer where

import Prelude

import Data.Array (concatMap, nub, sort)
import Data.Char (toCharCode)
import Data.Foldable (surroundMap)
import Data.Function (on)
import Data.Newtype (class Newtype)
import Data.String.CodeUnits (toCharArray)
import Partial.Unsafe (unsafePartial)
import Data.Array (unsafeIndex) as Array

newtype Token = Token Int

derive instance Newtype Token _
derive instance Eq Token
derive newtype instance Show Token

buildVocab :: Array String -> Array Char
buildVocab = sort <<< nub <<< concatMap toCharArray

bos :: Token
bos = Token 26

encode :: Char -> Token
encode c = Token $ on (-) toCharCode c 'a'

decode :: Array Char -> Token -> Char
decode vocab (Token t) = unsafePartial Array.unsafeIndex vocab t

tokenize :: Array String -> Array Token
tokenize docs = surroundMap [ bos ] (map encode <<< toCharArray) docs
