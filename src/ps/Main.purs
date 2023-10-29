module Main where

import Prelude

import Control.Monad.Except (Except, runExcept)
import Data.Either (Either(..))
import Data.List.Types (NonEmptyList)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple, fst, snd)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Random (randomInt)
import Flame (Html, QuerySelector(..), Subscription, (:>))
import Flame as App
import Flame.Html.Attribute (id, onClick)
import Flame.Html.Element (main, h1_, text, button, p_)
import Flame.Serialization (unsafeUnserialize)
import Flame.Subscription (onCustomEvent)
import Foreign (Foreign, ForeignError, readString, unsafeToForeign)
import Foreign.Index ((!))
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

-- recreating Elm type alias Decoder
type Decoder a = Foreign -> Except (NonEmptyList ForeignError) a

decoder :: Decoder TimeRecord
decoder value = do
  time <- value ! "time" >>= readString
  pure { time }

fromJson :: Foreign -> Msg
fromJson value =
  case runExcept (decoder value) of
    Right timeRecord -> GotTimeRecord timeRecord
    Left _ -> Randomize

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
  -- [ onCustomEvent (EventType "time") (\timeRecord -> GotTimeRecord timeRecord)
  [ onCustomEvent (EventType "time") (unsafeToForeign >>> fromJson)
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
  App.mount_ (QuerySelector "body")
    { init: (fst init) { count = initialCount } :> (snd init)
    , subscribe
    , update
    , view
    }
