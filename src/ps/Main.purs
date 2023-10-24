module Main where

import Prelude

import Effect (Effect)
import Flame (Html, QuerySelector(..), Subscription)
-- Side effects free updating; see docs for other examples
import Flame.Application.NoEffects as App
import Flame.Html.Element (main, button, text)
import Flame.Html.Attribute (onClick)

-- | The model represents the state of the app
type Model = Int

-- | Data type used to represent events
data Message = Increment | Decrement

-- | Initial state of the app
init :: Model
init = 0

-- | `update` is called to handle events
update :: Model -> Message -> Model
update model = case _ of
  Increment -> model + 1
  Decrement -> model - 1

-- | `view` is called whenever the model is updated
view :: Model -> Html Message
view model = main "main"
  [ button [ onClick Decrement ] "-"
  , text $ show model
  , button [ onClick Increment ] "+"
  ]

-- | Events that come from outside the `view`
subscribe :: Array (Subscription Message)
subscribe = []

-- | Mount the application on the given selector
start :: Effect Unit
start = App.mount_ (QuerySelector "body")
  { init
  , view
  , update
  , subscribe
  }
