module Components.MainPage (Query, State, Output, component) where

import Prelude

import Components.MessageModal (Query(..), State, Output, component) as MessageModal
import Control.Monad.State (get, put)
import Data.Const (Const)
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Halogen as H
import Halogen.Component as HC
import Halogen.HTML as HH
import Halogen.HTML.Events (onClick) as HE
import Halogen.HTML.Properties as HP
import Type.Proxy (Proxy(..))

type Query :: Type -> Type
type Query = Const Unit

type State =
    { status:: String
    }

type Input  = State
type Output = Void

data Action =
      ShowDialog
    | HandleMessageModal MessageModal.Output

type Slots = (child :: H.Slot MessageModal.Query MessageModal.Output Unit)

_child = Proxy :: Proxy "child"

component :: HC.Component Query Input Output Aff
component =
    HC.mkComponent
        { initialState: identity
        , render      : html
        , eval        : HC.mkEval $ HC.defaultEval
            { handleAction = handleAction
            }
        }

html :: State -> HH.ComponentHTML Action Slots Aff
html state =
    HH.div_
        [ HH.div
            [ HP.classes
                [ HH.ClassName "container"
                , HH.ClassName "section"
                ]
            ]
            [ HH.p
                [ HP.class_ $ HH.ClassName "center-align" ]
                [ HH.text state.status ]
            , HH.p
                [ HP.class_ $ HH.ClassName "center-align" ]
                [ HH.text "PureScript + Halogen + Materialize" ]
            , HH.div
                [ HP.class_ $ HH.ClassName "row" ]
                [ HH.a
                    [ HP.classes
                        [ HH.ClassName "btn"
                        , HH.ClassName "waves-effect"
                        , HH.ClassName "waves-light"
                        , HH.ClassName "pulse"
                        , HH.ClassName "col"
                        , HH.ClassName "s2"
                        , HH.ClassName "offset-s5"
                        , HH.ClassName "z-depth-5"
                        ]
                    , HE.onClick $ \_ -> ShowDialog
                    ]
                    [ HH.text "Hello" ]
                ]
            ]
        , HH.slot _child unit MessageModal.component childState HandleMessageModal
        ]
    where
        childState :: MessageModal.State
        childState =
            { modalMessage: "Hello, world!"
            , modalRef    : Nothing
            }

handleAction :: Action -> H.HalogenM State Action Slots Output Aff Unit
handleAction action =
    case action of
        ShowDialog -> H.request _child unit MessageModal.Open >>=
            traverse_
                ( \childState -> do
                    state <- get
                    put $ state
                        { status = state.status <> ". " <> childState.modalMessage
                        }
                )
        HandleMessageModal output -> do
            state <- get
            put $ state
                { status = output
                }
