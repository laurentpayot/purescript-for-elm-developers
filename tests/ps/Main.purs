module Test.Main
  ( Address
  , Contact
  , main
  ) where

import Prelude

import Effect (Effect)
import Test.Assert (assert)
import Data.Either (Either(..))
import Data.Validation.Semigroup (V, invalid, isValid)

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

nonEmptyEither :: String -> String -> Either String String
nonEmptyEither fieldName "" = Left $ "Field '" <> fieldName <> "' cannot be empty"
nonEmptyEither _ value = Right value

validateContactEither :: Contact -> Either String Contact
validateContactEither c = { firstName: _, lastName: _, address: _ }
  <$> nonEmptyEither "First Name" c.firstName
  <*> nonEmptyEither "Last Name" c.lastName
  <*> pure c.address

type ErrorMessages = Array String

nonEmptyV :: String -> String -> V ErrorMessages String
nonEmptyV fieldName "" = invalid [ "Field '" <> fieldName <> "' cannot be empty" ]
nonEmptyV _ value = pure value

validateContactV :: Contact -> V ErrorMessages Contact
validateContactV c = { firstName: _, lastName: _, address: _ }
  <$> nonEmptyV "First Name" c.firstName
  <*> nonEmptyV "Last Name" c.lastName
  <*> pure c.address

goodContact :: Contact
goodContact =
  { firstName: "John"
  , lastName: "Doe"
  , address:
      { street: "123 Main St."
      , city: "Springfield"
      , country: "USA"
      }
  }

badContact :: Contact
badContact =
  { firstName: ""
  , lastName: ""
  , address:
      { street: "123 Main St."
      , city: "Springfield"
      , country: "USA"
      }
  }

main :: Effect Unit
main = do

  assert $ (Score 4) + (Score 6) == (Score 10)

  -- applicative validation example testing

  assert $ validateContactEither goodContact ==
    Right goodContact

  assert $ validateContactEither badContact ==
    Left "Field 'First Name' cannot be empty"

  assert $ isValid $ validateContactV goodContact

  assert $ not isValid $ validateContactV badContact

  assert $ validateContactV badContact ==
    invalid
      [ "Field 'First Name' cannot be empty"
      , "Field 'Last Name' cannot be empty"
      ]
