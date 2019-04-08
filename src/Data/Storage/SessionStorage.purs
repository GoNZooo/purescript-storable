module Data.Storage.SessionStorage

( SessionStorage(..)
, runSessionStorage
)

where

import Prelude

import Effect (Effect)
import Effect.Class (class MonadEffect)

newtype SessionStorage a = SessionStorage (Effect a)

runSessionStorage :: ∀ a. SessionStorage a -> Effect a
runSessionStorage (SessionStorage ea) = ea

derive newtype instance functorSessionStorage     :: Functor SessionStorage
derive newtype instance applySessionStorage       :: Apply SessionStorage
derive newtype instance applicativeSessionStorage :: Applicative SessionStorage
derive newtype instance bindSessionStorage        :: Bind SessionStorage
derive newtype instance monadSessionStorage       :: Monad SessionStorage
derive newtype instance monadEffectSessionStorage :: MonadEffect SessionStorage