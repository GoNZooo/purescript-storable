module Data.Storage

( class MonadStorage
, store
, retrieve
, class Storable
, key
, prefix
, serialize
, deserialize
, StorageError(..)
, getKey
)

where

import Prelude

import Control.Monad.State (class MonadState, get, modify_)
import Data.Either (Either(..), either)
import Data.Generic.Rep as Rep
import Data.Generic.Rep.Show (genericShow)
import Data.List.Types (NonEmptyList)
import Data.Map (Map, insert, lookup)
import Data.Maybe (Maybe, maybe)
import Data.Storage.FFI (localStorage, sessionStorage)
import Data.Storage.LocalStorage (LocalStorage)
import Data.Storage.SessionStorage (SessionStorage)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Uncurried (EffectFn4, EffectFn6, runEffectFn4, runEffectFn6)
import Foreign (ForeignError)
import Type.Proxy (Proxy(..))
import Web.Storage.Storage (Storage)

class Storable a <= MonadStorage m a where
  store    :: a -> m (Either StorageError Unit)
  retrieve :: String -> m (Either StorageError a)

class Storable a where
  key         :: a -> String
  prefix      :: Proxy a -> String
  serialize   :: a -> String
  deserialize :: String -> Either StorageError a

data StorageError
  = NoValueError String
  | NoStorageError
  | DecodeError (NonEmptyList ForeignError)

derive instance genRepStorageError :: Rep.Generic StorageError _
instance showStorageError          :: Show StorageError where show = genericShow
derive instance eqStorageError     :: Eq StorageError

getKeys :: ∀ container item
  . Functor container
 => Storable item
 => container item
 -> container String
getKeys items = map getKey items

getKey :: ∀ a. Storable a => a -> String
getKey a = prefix (Proxy :: Proxy a) <> ":" <> key a

makeKey :: ∀ a. Storable a => Proxy a -> String -> String
makeKey p k = prefix p <> ":" <> k

instance monadStorageLocalStorage :: Storable a => MonadStorage LocalStorage a where
  store a    = liftEffect $ localStorage >>= maybeWriteToEffectStorage a
  retrieve k = liftEffect $ localStorage >>= maybeReadFromEffectStorage k

else instance monadStorageSessionStorage :: Storable a => MonadStorage SessionStorage a where
  store a    = liftEffect $ sessionStorage >>= maybeWriteToEffectStorage a
  retrieve k = liftEffect $ sessionStorage >>= maybeReadFromEffectStorage k

else instance monadStorageMonadState ::
  ( Storable a
  , MonadState (Map String String) m
  ) => MonadStorage m a where
  store a = do
    modify_ $ insert (getKey a) (serialize a)
    pure (Right unit)

  retrieve k = do
    state <- get
    maybe (pure $ Left $ NoValueError k) (pure <<< deserialize)
          (lookup (makeKey (Proxy :: Proxy a) k) state)

maybeWriteToEffectStorage :: ∀ a m
  . Storable a
 => MonadEffect m
 => a
 -> Maybe Storage
 -> m (Either StorageError Unit)
maybeWriteToEffectStorage a =
  maybe (pure $ Left NoStorageError) (writeToEffectStorage a)

maybeReadFromEffectStorage :: ∀ a m
  . Storable a
 => MonadEffect m
 => String
 -> Maybe Storage
 -> m (Either StorageError a)
maybeReadFromEffectStorage k =
  maybe (pure $ Left NoStorageError) (readFromEffectStorage k)

writeToEffectStorage :: ∀ a m
  . Storable a
 => MonadEffect m
 => a
 -> Storage
 -> m (Either StorageError Unit)
writeToEffectStorage a storage = setItem storage (getKey a) (serialize a)

readFromEffectStorage :: ∀ a m
  . Storable a
 => MonadEffect m
 => String
 -> Storage
 -> m (Either StorageError a)
readFromEffectStorage k storage =
  getItem storage (makeKey (Proxy :: Proxy a) k) >>=
    either (pure <<< Left) (pure <<< deserialize)

getItem :: ∀ m. MonadEffect m => Storage -> String -> m (Either StorageError String)
getItem storage key =
  liftEffect $ runEffectFn6 getItem_ NoStorageError NoValueError Left Right storage key

setItem :: ∀ m
  . MonadEffect m
 => Storage
 -> String
 -> String
 -> m (Either StorageError Unit)
setItem storage key value =
  liftEffect $ runEffectFn4 setItem_ NoStorageError storage key value

foreign import getItem_ ::
  EffectFn6 StorageError
            (String -> StorageError)
            (StorageError -> (Either StorageError String))
            (String -> (Either StorageError String))
            Storage
            String
            (Either StorageError String)

foreign import setItem_ ::
  EffectFn4 StorageError
            Storage
            String
            String
            (Either StorageError Unit)
