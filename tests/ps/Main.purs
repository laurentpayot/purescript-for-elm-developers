-- tests for the README examples

module Test.Main (main) where

import Prelude

import Data.Array.NonEmpty (uncons)
import Data.Array.NonEmpty.Internal (NonEmptyArray(..))
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Tuple (Tuple(..), fst, snd)
import Data.Tuple.Nested (type (/\), (/\), get2)
import Data.Validation.Semigroup (V, invalid, isValid)
import Effect (Effect)
import Effect.Aff (Milliseconds(..), delay, forkAff, joinFiber, launchAff_, suspendAff)
import Effect.Class (liftEffect)
import Effect.Console (logShow, log)
import Effect.Random (random)
import Effect.Timer (setTimeout, clearTimeout)
import Test.Assert (assert)

coords2D :: Tuple Int Int
coords2D = Tuple 10 20

getX :: Tuple Int Int -> Int
getX coords = fst coords

getY :: Tuple Int Int -> Int
getY coords = snd coords

coords3D :: Int /\ Int /\ Int
coords3D = 10 /\ 20 /\ 30

getY' :: Int /\ Int /\ Int -> Int
getY' coords = get2 coords

distance2D :: Tuple Int Int -> Int
distance2D (Tuple x y) =
  x * x + y * y

distance3D :: Int /\ Int /\ Int -> Int
distance3D (x /\ y /\ z) =
  x * x + y * y + z * z

neaHead :: NonEmptyArray Int -> Int
neaHead nonEmptyArray = _.head $ uncons nonEmptyArray

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

myGCD :: Int -> Int -> Int
myGCD n m
  | n == 0 = m
  | m == 0 = n
  | n > m = myGCD (n - m) m
  | otherwise = myGCD (m - n) n

main :: Effect Unit
main = do

  assert $ getX coords2D == 10
  assert $ getY coords2D == 20
  assert $ getY' coords3D == 20
  assert $ distance2D coords2D == 500
  assert $ distance3D coords3D == 1400

  assert $ neaHead (NonEmptyArray [ 1, 2, 3 ]) == 1

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

  log "Generating random number..."
  void random

  launchAff_ do
    timeoutID <- liftEffect $ setTimeout 1000 (log "This will run after 1 second")

    delay (Milliseconds 1300.0)

    liftEffect do
      log "Now cancelling timeout"
      clearTimeout timeoutID

  launchAff_ do

    fiber1 <- forkAff do
      liftEffect $ log "Fiber 1: Waiting for 1 second until completion."
      delay $ Milliseconds 1000.0
      liftEffect $ log "Fiber 1: Finished computation."

    fiber2 <- forkAff do
      liftEffect $ log "Fiber 2: Computation 1 (takes 300 ms)."
      delay $ Milliseconds 300.0
      liftEffect $ log "Fiber 2: Computation 2 (takes 300 ms)."
      delay $ Milliseconds 300.0
      liftEffect $ log "Fiber 2: Computation 3 (takes 500 ms)."
      delay $ Milliseconds 500.0
      liftEffect $ log "Fiber 2: Finished computation."

    fiber3 <- forkAff do
      liftEffect $ log "Fiber 3: Nothing to do. Just return immediately."
      liftEffect $ log "Fiber 3: Finished computation."

    joinFiber fiber1
    liftEffect $ log "Fiber 1 has finished. Now joining on fiber 2"
    joinFiber fiber2
    liftEffect $ log "Fiber 3 has finished. Now joining on fiber 3"
    joinFiber fiber3
    liftEffect $ log "Fiber 3 has finished. All fibers have finished their computation."

  log "_______________________________________"

  launchAff_ do

    fiber1 <- suspendAff do
      liftEffect $ log "Fiber 1: Waiting for 1 second until completion."
      delay $ Milliseconds 1000.0
      liftEffect $ log "Fiber 1: Finished computation."

    fiber2 <- suspendAff do
      liftEffect $ log "Fiber 2: Computation 1 (takes 300 ms)."
      delay $ Milliseconds 300.0
      liftEffect $ log "Fiber 2: Computation 2 (takes 300 ms)."
      delay $ Milliseconds 300.0
      liftEffect $ log "Fiber 2: Computation 3 (takes 500 ms)."
      delay $ Milliseconds 500.0
      liftEffect $ log "Fiber 2: Finished computation."

    fiber3 <- suspendAff do
      liftEffect $ log "Fiber 3: Nothing to do. Just return immediately."
      liftEffect $ log "Fiber 3: Finished computation."

    joinFiber fiber1
    liftEffect $ log "Fiber 1 has finished. Now joining on fiber 2"
    joinFiber fiber2
    liftEffect $ log "Fiber 3 has finished. Now joining on fiber 3"
    joinFiber fiber3
    liftEffect $ log "Fiber 3 has finished. All fibers have finished their computation."

  assert $ myGCD 15 20 == 5
  assert $ myGCD 20 15 == 5
  assert $ myGCD 15 15 == 15
