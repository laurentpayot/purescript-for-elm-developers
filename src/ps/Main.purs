module Main where

import Prelude

import Effect (Effect)
import Flame (Html, QuerySelector(..), Subscription)
-- Side effects free updating; see docs for other examples
import Flame.Application.NoEffects as App
import Flame.Html.Element (main, button, text)
import Flame.Html.Attribute (onClick)

-- | The model represents the state of the app
type Model =
  { counter :: Int
  }

type Flags =
  { counterInitialValue :: Int
  }

-- | Data type used to represent events
data Message
  = Increment
  | Decrement

-- | Initial state of the app
init :: Model
init =
  { counter: 0
  }

-- | `update` is called to handle events
update :: Model -> Message -> Model
update model = case _ of
  Increment -> model { counter = model.counter + 1 }
  Decrement -> model { counter = model.counter - 1 }

-- | `view` is called whenever the model is updated
view :: Model -> Html Message
view model = main "main"
  [ button [ onClick Decrement ] "-"
  , text $ show model.counter
  , button [ onClick Increment ] "+"
  ]

-- | Events that come from outside the `view`
subscribe :: Array (Subscription Message)
subscribe = []

-- | Mount the application on the given selector
start :: Flags -> Effect Unit
start flags = App.mount_ (QuerySelector "body")
  { init: init { counter = flags.counterInitialValue }
  , view
  , update
  , subscribe
  }
