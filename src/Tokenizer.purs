module Tokenizer where

import Prelude

import Data.Array (concatMap, length, nub, sort)
import Data.Char (toCharCode)
import Data.Foldable (surroundMap)
import Data.Function (on)
import Data.String.CodeUnits (toCharArray)
import Partial.Unsafe (unsafePartial)
import Data.Array (unsafeIndex) as Array

buildVocab :: Array String -> Array Char
buildVocab = sort <<< nub <<< concatMap toCharArray

decode :: Array Char -> Int -> Char
decode = unsafePartial Array.unsafeIndex

bos :: Int
bos = 26

encode :: Char -> Int
encode c = on (-) toCharCode c 'a'

tokenize :: Array String -> Array Int
tokenize docs = surroundMap [ bos ] (map encode <<< toCharArray) docs
