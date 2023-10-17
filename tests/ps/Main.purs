module Test.Main where

import Prelude

import Effect (Effect)
import Test.Assert (assert)

type Name =
  { firstName :: String
  , lastName :: String
  }

main :: Effect Unit
main = do
  assert true
