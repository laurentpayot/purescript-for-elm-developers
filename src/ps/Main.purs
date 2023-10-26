module Main where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Random (randomInt)
import Effect.Timer as Timer

import Flame (AppId(..), Html, QuerySelector(..), Subscription, (:>)) -- `:>` is an infix tuple constructor
import Flame as App
import Flame.Html.Attribute (onClick, id)
import Flame.Html.Element (main, h1, button, text)
import Flame.Subscription as Subscription
import Flame.Subscription.Document as Document

type Model =
  { roll ∷ Maybe Int
  , from ∷ String
  }

type Flags =
  { interval :: Int
  }

-- recreating Elm type alias `Cmd`
type Cmd msg = Aff (Maybe msg)

init ∷ Tuple Model (Array (Cmd Msg))
init =
  { roll: Nothing
  , from: ""
  } :> []

data Msg
  = IntervalRoll
  | ClickRoll
  | Update String Int

update ∷ Model -> Msg -> Tuple Model (Array (Cmd Msg))
update model = case _ of
  IntervalRoll -> model :> next "interval"
  ClickRoll -> model :> next "click"
  Update from int ->
    { roll: Just int
    , from
    } :> []
  where
  next :: String -> Array (Cmd Msg)
  next from = [ Just <<< Update from <$> liftEffect (randomInt 1 6) ]

subscribe ∷ Array (Subscription Msg)
subscribe =
  [ Document.onClick ClickRoll -- `document` click event
  ]

view ∷ Model -> Html Msg
view { roll, from } =
  main [ id "main" ]
    [ h1 [ id "foo" ] [ text "Dice Rolling" ]
    , text $ case roll of
        Nothing -> "No rolls!"
        Just r -> "Roll from " <> from <> ": " <> show r
    ]

start ∷ Flags -> Effect Unit
start flags = do
  let id = AppId "dice-rolling"
  App.mount (QuerySelector "body") id
    { init
    , subscribe
    , update
    , view
    }
  -- roll dice every interval
  void $ Timer.setInterval flags.interval (Subscription.send id IntervalRoll)
