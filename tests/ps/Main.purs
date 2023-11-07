module Test.Main (main) where

import Prelude

import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Validation.Semigroup (V, invalid, isValid)
import Effect (Effect)
import Effect.Console (logShow, log)
import Effect.Random (random)
import Test.Assert (assert)

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
badContact = goodContact { firstName = "", lastName = "" }

nonEmptyEither :: String -> String -> Either String String
nonEmptyEither fieldName value
  | value == "" = Left $ "Field '" <> fieldName <> "' cannot be empty"
  | otherwise = Right value

validateContactEither :: Contact -> Either String Contact
validateContactEither c = { firstName: _, lastName: _, address: _ }
  <$> nonEmptyEither "First Name" c.firstName
  <*> nonEmptyEither "Last Name" c.lastName
  <*> pure c.address

type ErrorMessages = Array String

nonEmptyV :: String -> String -> V ErrorMessages String
nonEmptyV fieldName value
  | value == "" = invalid [ "Field '" <> fieldName <> "' cannot be empty" ]
  | otherwise = pure value

validateContactV :: Contact -> V ErrorMessages Contact
validateContactV c = { firstName: _, lastName: _, address: _ }
  <$> nonEmptyV "First Name" c.firstName
  <*> nonEmptyV "Last Name" c.lastName
  <*> pure c.address

validateContactVAdo :: Contact -> V ErrorMessages Contact
validateContactVAdo c = ado
  firstName <- nonEmptyV "First Name" c.firstName
  lastName <- nonEmptyV "Last Name" c.lastName
  address <- pure c.address
  in { firstName, lastName, address }

data MyADT
  = Some
  | Arbitrary Int
  | Contents Number String

derive instance Eq MyADT
derive instance Ord MyADT
derive instance Generic MyADT _

instance Show MyADT where
  show = genericShow

showCompare :: forall a. Ord a => Show a => a -> a -> String
showCompare a1 a2
  | a1 < a2 = show a1 <> " is less than " <> show a2
  | a1 > a2 = show a1 <> " is greater than " <> show a2
  | otherwise = show a1 <> " is equal to " <> show a2

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

  assert $ isValid $ validateContactVAdo goodContact

  assert $ not isValid $ validateContactVAdo badContact

  assert $ validateContactVAdo badContact ==
    invalid
      [ "Field 'First Name' cannot be empty"
      , "Field 'Last Name' cannot be empty"
      ]

  -- logs `[Some,(Arbitrary 1),(Contents 2.0 "Three")]`
  logShow [ Some, Arbitrary 1, Contents 2.0 "Three" ]

  log $ showCompare 1 2
  log $ showCompare 2 1
  log $ showCompare 1 1

  (log "Below is a random number between 0.0 and 1.0:") >>=
    ( \_ ->
        random >>=
          ( \n ->
              log $ show n
          )
    )

  log "Below is a random number between 0.0 and 1.0:"
  n <- random
  log $ show n
