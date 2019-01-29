module KVSpec where

import           KV.Server
import           KV.Store.Mem
import           Network.Wai
import           System.Random
import           Test.Hspec
import           Test.Hspec.Wai

app :: IO Application
app = makeStore (mkStdGen 1) >>= server

spec :: Spec
spec = with app $ describe "Store Server" $ do

  describe "on POST /"  $ do
    it "responds with key when sent a value" $ do
      post "123" "/" `shouldRespondWith` "" {matchStatus = 201}
