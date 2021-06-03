module Components.MessageModal (Query(..), State, Input, Output, Action(..), component) where

import Prelude

import CSS (px, width) as CSS
import Control.Monad.State (get, put)
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import FFI.Materialize (initModal, showModal)
import Halogen (liftAff)
import Halogen as H
import Halogen.Component as HC
import Halogen.HTML as HH
import Halogen.HTML.CSS (style) as CSS
import Halogen.HTML.Properties as HP
import Web.HTML (HTMLElement)

data Query :: Type -> Type
data Query a = Open (State -> a)

type State =
    { modalMessage :: String
    , modalRef     :: Maybe HTMLElement
    }

type Input  = State
type Output = String

data Action = Initialize

type Slots :: forall k. Row k
type Slots = ()

component :: HC.Component Query Input Output Aff
component =
    HC.mkComponent
        { initialState: identity
        , render      : html
        , eval        : HC.mkEval $ HC.defaultEval
            { initialize   = Just Initialize
            , handleQuery  = handleQuery
            , handleAction = handleAction
            , receive      = const Nothing
            }
        }

html :: State -> HH.ComponentHTML Action Slots Aff
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

handleQuery :: forall a. Query a -> H.HalogenM State Action Slots Output Aff (Maybe a)
handleQuery query =
    case query of
        Open reply -> do
            state <- get
            traverse_ (\ref -> liftAff $ showModal ref) state.modalRef
            H.raise "Opened"
            pure $ Just $ reply state

handleAction :: Action -> H.HalogenM State Action Slots Output Aff Unit
handleAction action =
    case action of
        Initialize -> do
            H.getHTMLElementRef (H.RefLabel "message-modal") >>=
                traverse_ \ref -> do
                    liftAff $ initModal ref
                    state <- get
                    put $ state { modalRef = Just ref }
