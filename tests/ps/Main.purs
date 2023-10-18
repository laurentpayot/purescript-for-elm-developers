module Test.Main
  ( Address
  , Contact
  , toContact
  , main
  ) where

import Prelude

import Effect (Effect)
import Test.Assert (assert)
import Data.Either (Either(..))

newtype Score = Score Int

derive newtype instance Semiring Score
derive newtype instance Eq Score

type Contact =
  { firstName :: String
  , lastName :: String
  , address :: Address
  }

type Address =
  { street :: String
  , city :: String
  , country :: String
  }

-- Contact constructor
toContact :: String -> String -> Address -> Contact
toContact firstName lastName address =
  { firstName
  , lastName
  , address
  }

nonEmpty :: String -> Either String String
nonEmpty "" = Left "Field cannot be empty"
nonEmpty value = Right value

validateContact :: Contact -> Either String Contact
validateContact c = toContact
  <$> nonEmpty c.firstName
  <*> nonEmpty c.lastName
  <*> pure c.address

goodContact :: Contact
goodContact = toContact "John" "Doe" { street: "123 Main St.", city: "Anytown", country: "USA" }

badContact :: Contact
badContact = toContact "" "" { street: "123 Main St.", city: "Anytown", country: "USA" }

main :: Effect Unit
main = do

  assert $ (Score 4) + (Score 6) == (Score 10)

  -- applicative validation example testing

  assert $
    validateContact goodContact == Right { firstName: "John", lastName: "Doe", address: { street: "123 Main St.", city: "Anytown", country: "USA" } }

  assert $
    validateContact badContact == Left "Field cannot be empty"
