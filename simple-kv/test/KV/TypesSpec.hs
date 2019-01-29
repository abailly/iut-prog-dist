{-# LANGUAGE TypeApplications #-}
module KV.TypesSpec where

import           Data.Aeson
import           KV.Types
import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

spec :: Spec
spec = describe "Core Types" $ do

  it "can encode/decode Values to/from JSON" $ property $ canJSONEncodeDecode @Values

instance Arbitrary Values where
  arbitrary = Values <$> arbitrary

canJSONEncodeDecode :: (Eq a, Show a, ToJSON a, FromJSON a)
                    => a -> Bool
canJSONEncodeDecode a =
  decode (encode a) == Just a
