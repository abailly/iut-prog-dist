module KV.StoreSpec where

import           KV.Store
import           KV.Types
import           System.Random
import           Test.Hspec

spec :: Spec
spec = describe "Store Ops" $ do

  let store = emptyStore baseSeed
      baseSeed = mkStdGen 1
      nextSeed = snd $ split baseSeed

  it "generate key when creating value" $ do
    act (Create "123") store `shouldBe` Stored (Key "a7f6112f4a4b0a0b") nextSeed "123"

  it "use provided key when modifying value" $ do
    act (Modify "abc" "123") store `shouldBe` Stored (Key "abc") baseSeed "123"

  it "update store with value when creating" $ do
    let event = act (Create "123") store
        store' = apply event store

    retrieve "a7f6112f4a4b0a0b" store' `shouldBe` Just "123"

  it "returns Nothing when retrieving given key does not exist" $ do
    retrieve "a7f6112f4a4b0a0b" store `shouldBe` Nothing
