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

  let
    postJson path body = request methodPost path [ ("content-type", "application/json") ] body
    putJson path body = request methodPut path [ ("content-type", "application/json") ] body
    getJson path = request methodGet path [ ("content-type", "application/json") ] ""

  describe "on POST /"  $ do

    it "responds with key when sent a value" $ do
      postJson "/" "{\"data\":\"1234\"}"
        `shouldRespondWith` "\"a7f6112f4a4b0a0b\"" {matchStatus = 201}

  describe "on PUT /<key>"  $ do
    it "responds with key when sent a value with a key" $ do
      putJson "/a7f6112f4a4b0a0b" "{\"data\":\"1234\"}"
        `shouldRespondWith` 200

  describe "on GET /<key>"  $ do

    it "responds with value when sent a key given value exists" $ do
      void $ putJson "/a7f6112f4a4b0a0b" "{\"data\":\"1234\"}"

      getJson "/a7f6112f4a4b0a0b"
        `shouldRespondWith` "{\"data\":\"1234\"}"

    it "responds with 404 when sent a key given value does not exist" $ do
      getJson "/a7f6112f4a4b0a0b"
        `shouldRespondWith` 404

  describe "on GET /"  $ do

    it "list all available values" $ do
      void $ putJson "/a7f6112f4a4b0a0b" "{\"data\":\"1234\"}"

      getJson "/"
        `shouldRespondWith` "[{\"data\":\"1234\"}]"

  describe "on GET /swagger.json"  $ do
    it "returns a Swagger descriptor for API" $ do
      getJson "/swagger.json" `shouldRespondWith` 200

  describe "on DELETE /"  $ do

    it "clear all values" $ do
      -- arrange
      void $ putJson "/a7f6112f4a4b0a0b" "{\"data\":\"1234\"}"

      -- act
      delete  "/" `shouldRespondWith` 200

      -- assert
      getJson "/a7f6112f4a4b0a0b" `shouldRespondWith` 404
