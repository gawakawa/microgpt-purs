module Test.Main where

import Prelude

import Effect (Effect)
import Test.Autograd as Autograd
import Test.GPT as GPT
import Test.Tokenizer as Tokenizer
import Test.Train as Train
import Test.Unit.Main (runTest)

main :: Effect Unit
main = runTest do
  Autograd.tests
  GPT.tests
  Tokenizer.tests
  Train.tests
