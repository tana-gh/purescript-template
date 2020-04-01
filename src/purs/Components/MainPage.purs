module Components.MainPage (Query, State, Message, component) where

import Prelude

import Components.MessageModal (Query(..), State, Message, component) as MessageModal
import Data.Const (Const)
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Effect.Aff (Aff)
import FFI.Materialize (showModal)
import Halogen (liftAff)
import Halogen as H
import Halogen.Component as HC
import Halogen.HTML as HH
import Halogen.HTML.Events (onClick) as HE
import Halogen.HTML.Properties as HP
import Record (insert) as Record

type Query = Const Unit

type State =
    { modalMessage :: String
    }

type Message = Void

data Action = ShowDialog

type ChildSlots = (child :: H.Slot MessageModal.Query MessageModal.Message Unit)

child_ = SProxy :: SProxy "child"

component :: HC.Component HH.HTML Query State Message Aff
component =
    HC.mkComponent
        { initialState: identity
        , render      : html
        , eval        : HC.mkEval $ HC.defaultEval
            { handleAction = handleAction
            }
        }

html :: State -> HH.ComponentHTML Action ChildSlots Aff
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
                    , HE.onClick $ \_ -> Just ShowDialog
                    ]
                    [ HH.text "Hello" ]
                ]
            ]
        , HH.slot child_ unit MessageModal.component (toChildState state) (const Nothing)
        ]
    where
        toChildState :: State -> MessageModal.State
        toChildState = Record.insert (SProxy :: SProxy "modalRef") Nothing

handleAction :: Action -> H.HalogenM State Action ChildSlots Message Aff Unit
handleAction action =
    case action of
        ShowDialog -> H.query child_ unit (H.request MessageModal.GetRef) >>=
            traverse_ (\childState -> traverse_ (\ref -> liftAff $ showModal ref) childState.modalRef)
