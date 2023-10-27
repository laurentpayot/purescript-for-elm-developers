module Main
  ( Cmd
  , Model
  , Msg(..)
  , init
  , start
  , subscribe
  , update
  , view
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple, fst, snd)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Random (randomInt)
import Flame (AppId(..), Html, QuerySelector(..), Subscription, (:>))
import Flame as App
import Flame.Html.Attribute (id, onClick)
import Flame.Html.Element (main, h1_, text, button, p_)
import Flame.Subscription (onCustomEvent)
import Web.Event.Event (EventType(..))

type Model =
  { count :: IntCustomEvent
  , time :: String
  }

type Flags =
  { initialCount :: Int
  }

-- recreating Elm type alias `Cmd`
type Cmd msg = Aff (Maybe msg)

init :: Tuple Model (Array (Cmd Msg))
init =
  { count: 0, time: "Waiting for tick…" } :> []

data Msg
  = Increment
  | Decrement
  | Randomize
  | GotRandom Int
  | GotTick String

update ∷ Model -> Msg -> Tuple Model (Array (Cmd Msg))
update model = case _ of
  Increment -> model { count = model.count + 1 } :> []
  Decrement -> model { count = model.count - 1 } :> []
  Randomize -> model :> [ Just <<< GotRandom <$> liftEffect (randomInt 1 100) ]
  GotRandom int -> model { count = int } :> []
  GotTick timeStr -> model { time = timeStr } :> []

subscribe ∷ Array (Subscription Msg)
subscribe =
  [ -- onCustomEvent (EventType "tick") GotTick
  ]

view ∷ Model -> Html Msg
view { count, time } =
  main [ id "main" ]
    [ h1_ "Flame example"
    , button [ onClick Decrement ] "-"
    , text (show count)
    , button [ onClick Increment ] "+"
    , p_
        [ button [ onClick Randomize ] "Random"
        ]
    , p_ time
    ]

start ∷ Flags -> Effect Unit
start { initialCount } = do
  App.mount (QuerySelector "body") (AppId "flame-example")
    { init: (fst init) { count = initialCount } :> (snd init)
    , subscribe
    , update
    , view
    }
