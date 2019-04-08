module Test.StorageSpec

( storageSpec
)

where

import Prelude

import Control.Monad.State (StateT, evalStateT)
import Data.Either (Either(..))
import Data.Map (Map)
import Data.Map as Map
import Effect (Effect)
import Effect.Class (liftEffect)
import Test.Data.Note (Note(..))
import Data.Storage (StorageError, retrieve, store)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

storageSpec :: Spec Unit
storageSpec =
  describe "Storage" $
    describe "StateMonad Note Storage" $
      it "Stores a note and retrieves it correctly" do
        let note = Note {title: "title", body: "body", id: 42}
        stateResult <- liftEffect $ evalStateT (stringMapNoteTest note) Map.empty
        (stateResult `shouldEqual` (Right note))

stringMapNoteTest :: Note -> StateT (Map String String) Effect (Either StorageError Note)
stringMapNoteTest note@(Note {id}) = do
  _ <- store note
  retrieve (show id)