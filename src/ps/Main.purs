module Main where

import Prelude

import Data.Maybe (Maybe(..), fromMaybe)
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

-- import Debug (spy)

type Model =
  { count :: Int
  , time :: String
  }

type Flags =
  { initialCount :: Int
  }

type TimeRecord =
  { time :: String
  }

-- recreating Elm type alias `Cmd`
type Cmd msg = Aff (Maybe msg)

init :: Tuple Model (Array (Cmd Msg))
init =
  { count: 0
  , time: "Waiting for time…"
  } :> []

data Msg
  = Increment
  | Decrement
  | Randomize
  | GotRandom Int
  | GotTimeRecord TimeRecord

update ∷ Model -> Msg -> Tuple Model (Array (Cmd Msg))
update model = case _ of
  Increment -> model { count = model.count + 1 } :> []
  Decrement -> model { count = model.count - 1 } :> []
  Randomize -> model :> [ Just <<< GotRandom <$> liftEffect (randomInt 1 100) ]
  GotRandom int -> model { count = int } :> []
  GotTimeRecord { time } -> model { time = time } :> []

subscribe ∷ Array (Subscription Msg)
subscribe =
  [ onCustomEvent (EventType "time") (fromMaybe { time: "???" } >>> GotTimeRecord)
  ]

view ∷ Model -> Html Msg
view { count, time } =
  main [ id "main" ]
    [ h1_ "Flame example"
    , button [ onClick Decrement ] "-"
    , text (show count)
    , button [ onClick Increment ] "+"
    , p_ [ button [ onClick Randomize ] "Random" ]
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
