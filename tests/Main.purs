module Test.Main where

import Prelude

import Effect (Effect)
import Test.Assert (assert)

import Main as Foo

main :: Effect Unit
main = do
  assert (1 == 1)
