module Data.Storage.FFI

( localStorage
, sessionStorage
, keys
)

where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Uncurried (EffectFn1, EffectFn2, runEffectFn1, runEffectFn2)
import Web.Storage.Storage (Storage)

localStorage :: Effect (Maybe Storage)
localStorage = runEffectFn2 localStorage_ Just Nothing

sessionStorage :: Effect (Maybe Storage)
sessionStorage = runEffectFn2 sessionStorage_ Just Nothing

keys :: ∀ m. MonadEffect m => Storage -> m (Array String)
keys storage = liftEffect $ runEffectFn1 keys_ storage


foreign import localStorage_ :: ∀ a
  . EffectFn2 (a -> Maybe a)
              (Maybe a)
              (Maybe Storage)

foreign import sessionStorage_ :: ∀ a
  . EffectFn2 (a -> Maybe a)
              (Maybe a)
              (Maybe Storage)

foreign import keys_ :: EffectFn1 Storage (Array String)
