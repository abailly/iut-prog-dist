{-# LANGUAGE OverloadedStrings #-}
module KV.ServerSpec where

import           Data.Functor              (void)
import           KV.Server
import           KV.Store.Mem
import           Network.HTTP.Types.Method
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
      request methodPost "/" [ ("content-type", "application/json") ] "{\"data\":\"1234\"}"
        `shouldRespondWith` "\"a7f6112f4a4b0a0b\"" {matchStatus = 201}

  describe "on PUT /<key>"  $ do
    it "responds with key when sent a value with a key" $ do
      request methodPut "/a7f6112f4a4b0a0b" [ ("content-type", "application/json") ] "{\"data\":\"1234\"}"
        `shouldRespondWith` 200

  describe "on GET /<key>"  $ do

    it "responds with value when sent a key given value exists" $ do
      void $ request methodPut "/a7f6112f4a4b0a0b" [ ("content-type", "application/json") ] "{\"data\":\"1234\"}"

      request methodGet "/a7f6112f4a4b0a0b" [ ("content-type", "application/json") ] ""
        `shouldRespondWith` "{\"data\":\"1234\"}"

    it "responds with 404 when sent a key given value does not exist" $ do
      request methodGet "/a7f6112f4a4b0a0b" [ ("content-type", "application/json") ] ""
        `shouldRespondWith` 404
