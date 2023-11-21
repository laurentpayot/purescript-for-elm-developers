module Main where

import Prelude

import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Tuple (fst, snd)
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Random (randomInt)

import Flame (Html, QuerySelector(..), Subscription)
import Flame as App
import Flame.Html.Attribute (id, onClick, src, height)
import Flame.Html.Element (main, h1_, text, button, p_, img)
import Flame.Subscription (onCustomEvent)
import Promise.Aff (Promise, toAffE)
import Web.Event.Event (EventType(..))

-- import Debug (spy)

foreign import multiply :: Int -> Int -> Int
foreign import catBase64JS :: String -> Int -> Effect (Promise String)

catBase64 :: String -> Int -> Aff String
catBase64 text fontSize = toAffE $ catBase64JS text fontSize

type Model =
  { count :: Int
  , time :: String
  , cat :: Maybe String
  }

type Flags =
  { initialCount :: Int
  }

type TimeRecord =
  { time :: String
  }

-- recreating Elm type alias `Cmd`
type Cmd msg = Aff (Maybe msg)

init :: Model /\ (Array (Cmd Msg))
init =
  { count: 0
  , time: "Waiting for time…"
  , cat: Nothing
  } /\ []

data Msg
  = Increment
  | Decrement
  | Randomize
  | GotRandom Int
  | GotTimeRecord TimeRecord
  | DoubleCount
  | GetCat
  | GotCat String

update ∷ Model -> Msg -> Model /\ (Array (Cmd Msg))
update model@{ count } = case _ of
  Increment -> model { count = count + 1 } /\ []
  Decrement -> model { count = count - 1 } /\ []
  Randomize -> model /\ [ Just <<< GotRandom <$> liftEffect (randomInt 1 100) ]
  GotRandom int -> model { count = int } /\ []
  GotTimeRecord { time } -> model { time = time } /\ []
  DoubleCount -> model { count = multiply count 2 } /\ []
  GetCat -> model /\ [ Just <<< GotCat <$> catBase64 (show count) 100 ]
  GotCat base64 -> model { cat = Just base64 } /\ []

subscribe ∷ Array (Subscription Msg)
subscribe =
  [ onCustomEvent (EventType "time") (\timeRecord -> GotTimeRecord timeRecord)
  ]

view ∷ Model -> Html Msg
view { count, time, cat } =
  main [ id "main" ]
    [ h1_ "Flame example"
    , p_ time
    , button [ onClick Decrement ] "-"
    , text (show count)
    , button [ onClick Increment ] "+"
    , p_
        [ button [ onClick DoubleCount ] "Double"
        , button [ onClick Randomize ] "Random"
        ]
    , p_ [ button [ onClick GetCat ] "Cat" ]
    , if isJust cat then
        p_ [ img [ src ("data:image/png;base64," <> fromMaybe "" cat), height "200px" ] ]
      else
        p_ "No cat yet"
    ]

start ∷ Flags -> Effect Unit
start { initialCount } = do
  App.mount_ (QuerySelector "body")
    { init: (fst init) { count = initialCount } /\ (snd init)
    , subscribe
    , update
    , view
    }
