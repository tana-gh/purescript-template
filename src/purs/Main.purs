module Main (main) where

import Prelude

import Components.MainPage (State, component) as MainPage
import Data.Foldable (traverse_)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Halogen.Aff.Util (awaitLoad, selectElement)
import Halogen.VDom.Driver (runUI)
import Web.DOM.ParentNode (QuerySelector(..))

main :: Effect Unit
main = launchAff_ do
    awaitLoad
    selectElement (QuerySelector "#app") >>=
        traverse_ \app -> runUI MainPage.component initialState app

initialState :: MainPage.State
initialState =
    { status: "Not opened"
    }
