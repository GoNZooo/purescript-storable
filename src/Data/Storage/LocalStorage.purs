module Data.Storage.LocalStorage

( LocalStorage(..)
, runLocalStorage
)

where

import Prelude

import Effect (Effect)
import Effect.Class (class MonadEffect)

newtype LocalStorage a = LocalStorage (Effect a)

runLocalStorage :: ∀ a. LocalStorage a -> Effect a
runLocalStorage (LocalStorage ea) = ea

derive newtype instance functorLocalStorage     :: Functor LocalStorage
derive newtype instance applyLocalStorage       :: Apply LocalStorage
derive newtype instance applicativeLocalStorage :: Applicative LocalStorage
derive newtype instance bindLocalStorage        :: Bind LocalStorage
derive newtype instance monadLocalStorage       :: Monad LocalStorage
derive newtype instance monadEffectLocalStorage :: MonadEffect LocalStorage