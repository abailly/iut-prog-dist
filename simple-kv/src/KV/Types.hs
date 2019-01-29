{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
module KV.Types where

import           Data.Aeson
import           Data.ByteString        as BS
import           Data.ByteString.Base16 as Hex
import qualified Data.ByteString.Base64 as Base64
import           Data.String
import           Data.Text              as Text
import           Data.Text.Encoding     (decodeUtf8, encodeUtf8)
import           GHC.Generics
import           Servant
import           System.Random          as Random

newtype Key = Key { unKey :: Text }
  deriving (Eq, Ord, Read, Generic)

instance Show Key where
  show (Key k) = Text.unpack k

deriving newtype instance ToJSON Key
deriving newtype instance FromJSON Key
deriving newtype instance IsString Key

instance Random Key where
  randomR = error "not implemented"
  random g =
    let (_, g') = Random.split g
        k = Key $ decodeUtf8 $ Hex.encode $ BS.pack $ Prelude.take 8 $ randoms g
    in (k, g')

instance FromHttpApiData Key where
  parseQueryParam = Right . Key

instance ToHttpApiData Key where
  toQueryParam (Key u) = u

newtype Bytes = Bytes { bytes :: ByteString }
  deriving (Eq, Show, Read, Generic)

deriving newtype instance IsString Bytes

instance ToJSON Bytes where
  toJSON (Bytes bs) = object [ "data" .= decodeUtf8 (Base64.encode bs) ]

instance FromJSON Bytes where
  parseJSON = withObject "Bytes" $ \ o -> either fail (pure . Bytes) =<< (Base64.decode . encodeUtf8) <$> o.: "data"

data Command = Create { value :: Bytes }
             | Modify { key :: Key, value :: Bytes }
  deriving (Eq, Show, Read, Generic, ToJSON, FromJSON)


data Event = Stored { storedKey :: Key, seed :: StdGen, value :: Bytes }
           | Error { reason :: Text }
  deriving (Eq, Show, Read, Generic, ToJSON, FromJSON)

instance Eq StdGen where
  s == s' = show s == show s'

instance ToJSON StdGen where
  toJSON s = String $ Text.pack $ show s

instance FromJSON StdGen where
  parseJSON = withText "StdGen" $ pure . read . Text.unpack
