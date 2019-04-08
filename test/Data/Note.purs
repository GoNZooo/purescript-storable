module Test.Data.Note

( Note(..)
)

where

import Prelude

import Data.Either (Either(..), either)
import Data.Generic.Rep as Rep
import Data.Generic.Rep.Show (genericShow)
import Data.Storage (class Storable, StorageError(..))
import Simple.JSON (class ReadForeign, class WriteForeign, readJSON, writeJSON)
import Test.QuickCheck (class Arbitrary)

newtype Note =
  Note { title :: String
       , body  :: String
       , id    :: Int
       }

derive instance repGenericNote           :: Rep.Generic Note _
instance showNote                        :: Show Note where show = genericShow
derive newtype instance readForeignNote  :: ReadForeign Note
derive newtype instance writeForeignNote :: WriteForeign Note
derive instance eqNote                   :: Eq Note
derive newtype instance arbitraryNote    :: Arbitrary Note

instance storableNote :: Storable Note where
  prefix _        = "Note" 
  key (Note {id}) = show id
  serialize n     = writeJSON n
  deserialize     = either (Left <<< DecodeError) Right <<< readJSON