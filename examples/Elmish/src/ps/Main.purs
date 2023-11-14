module Main where

import Prelude
import Effect (Effect)

import Elmish (Transition, Dispatch, ReactElement, (<|))
import Elmish.HTML.Events as E  -- This is more convenient to import qualified
import Elmish.HTML.Styled as H  -- This is more convenient to import qualified
import Elmish.Boot (defaultMain)

-- import Debug (spy)


data Message

type State = Unit

init :: Transition Message State
init = pure unit

update :: State -> Message -> Transition Message State
update _ _ = pure unit

view :: State -> Dispatch Message -> ReactElement
view _ _ =
  H.div "p-4"
  [ H.text "Hello, "
  , H.strong "" "World!"
  ]


main :: Effect Unit
main = defaultMain { def: { init, view, update }, elementId: "root" }
