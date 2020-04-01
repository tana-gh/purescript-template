module Components.MessageModal (Query(..), State, Message, component) where

import Prelude

import CSS (px, width) as CSS
import Control.Monad.State (get, put)
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import FFI.Materialize (initModal)
import Halogen (liftAff)
import Halogen as H
import Halogen.Component as HC
import Halogen.HTML as HH
import Halogen.HTML.CSS (style) as CSS
import Halogen.HTML.Properties as HP
import Web.HTML (HTMLElement)

data Query a = GetRef (State -> a)

type State =
    { modalMessage :: String
    , modalRef     :: Maybe HTMLElement
    }

type Message = Void

data Action = Initialize

type ChildSlots = ()

component :: HC.Component HH.HTML Query State Message Aff
component =
    HC.mkComponent
        { initialState: identity
        , render      : html
        , eval        : HC.mkEval $ HC.defaultEval
            { initialize   = Just Initialize
            , handleQuery  = handleQuery
            , receive      = const Nothing
            , handleAction = handleAction
            }
        }

html :: State -> HH.ComponentHTML Action ChildSlots Aff
html state =
    HH.div
        [ HP.class_ $ HH.ClassName "modal"
        , HP.ref $ H.RefLabel "message-modal"
        , CSS.style do
            CSS.width $ CSS.px 200.0
        ]
        [ HH.div
            [ HP.class_ $ HH.ClassName "modal-content" ]
            [ HH.p_
                [ HH.text $ state.modalMessage ]
            ]
        , HH.div
            [ HP.class_ $ HH.ClassName "modal-footer" ]
            [ HH.a
                [ HP.classes
                    [ HH.ClassName "btn"
                    , HH.ClassName "waves-effect"
                    , HH.ClassName "waves-light"
                    , HH.ClassName "modal-close"
                    ]
                , CSS.style do
                    CSS.width $ CSS.px 80.0
                ]
                [ HH.text "OK" ]
            ]
        ]

handleQuery :: forall a. Query a -> H.HalogenM State Action ChildSlots Message Aff (Maybe a)
handleQuery query =
    case query of
        GetRef reply -> do
            state <- get
            pure $ Just $ reply state

handleAction :: Action -> H.HalogenM State Action ChildSlots Message Aff Unit
handleAction action =
    case action of
        Initialize -> H.getHTMLElementRef (H.RefLabel "message-modal") >>=
            traverse_ \ref -> do
                liftAff $ initModal ref
                state <- get
                put $ state { modalRef = Just ref }
