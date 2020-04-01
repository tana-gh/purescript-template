module FFI.Materialize (initModal, showModal) where

import Prelude

import Effect.Aff (Aff)
import Effect.Aff.Compat (EffectFnAff, fromEffectFnAff)
import Web.HTML (HTMLElement)

foreign import initModalJS :: HTMLElement -> EffectFnAff Unit
foreign import showModalJS :: HTMLElement -> EffectFnAff Unit

initModal :: HTMLElement -> Aff Unit
initModal element = fromEffectFnAff $ initModalJS element

showModal :: HTMLElement -> Aff Unit
showModal element = fromEffectFnAff $ showModalJS element
