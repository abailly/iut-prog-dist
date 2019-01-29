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

    act (Retrieve "a7f6112f4a4b0a0b") store' `shouldBe` Retrieved "a7f6112f4a4b0a0b"  "123"

  it "return an error when retrieving given key does not exist" $ do
    act (Retrieve "a7f6112f4a4b0a0b") store `shouldBe` Error "Key 'a7f6112f4a4b0a0b' not found"

  it "do not update store when retrieving" $ do
    apply (Retrieved "a7f6112f4a4b0a0b"  "123") store `shouldBe` store
