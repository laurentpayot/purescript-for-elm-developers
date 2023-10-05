module Test.Main where

import Prelude

import Effect (Effect)
import Test.Assert (assert)

import Euler (answer)


main :: Effect Unit
main = do
  assert (answer == 233168)
