{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
module KV.Types where

import           Data.Aeson
import           Data.ByteString
import qualified Data.ByteString.Base64 as Base64
import           Data.Text              as Text
import           Data.Text.Encoding     (decodeUtf8, encodeUtf8)
import           GHC.Generics
import           Servant

newtype Key = Key { unKey :: Text }
  deriving (Eq, Show, Read, Generic)

deriving newtype instance ToJSON Key
deriving newtype instance FromJSON Key

instance FromHttpApiData Key where
  parseQueryParam = Right . Key

instance ToHttpApiData Key where
  toQueryParam (Key u) = u

newtype Bytes = Bytes { bytes :: ByteString }
  deriving (Eq, Show, Read, Generic)

instance ToJSON Bytes where
  toJSON (Bytes bs) = String $ decodeUtf8 $ Base64.encode bs

instance FromJSON Bytes where
  parseJSON = withText "Bytes" $ \ t -> either fail (pure . Bytes) (Base64.decode $ encodeUtf8 $ t)

data Command = Create { value :: Bytes }
             | Modify { key :: Key, value :: Bytes }
             | Retrieve { key :: Key }
  deriving (Eq, Show, Read, Generic, ToJSON, FromJSON)


data Event = Stored { key :: Key, value :: Bytes }
           | Retrieved { key :: Key, value :: Bytes }
           | Error { reason :: Text }
  deriving (Eq, Show, Read, Generic, ToJSON, FromJSON)
