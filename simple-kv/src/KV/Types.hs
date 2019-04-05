{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
module KV.Types where

import           Control.Lens           hiding ((.=))
import           Data.Aeson
import           Data.ByteString        as BS
import           Data.ByteString.Base16 as Hex
import           Data.String
import           Data.Swagger
import           Data.Text              as Text
import           Data.Text.Encoding     (decodeUtf8)
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

instance ToParamSchema Key
instance ToSchema Key

instance FromHttpApiData Key where
  parseQueryParam = Right . Key

instance ToHttpApiData Key where
  toQueryParam (Key u) = u

newtype Values = Values { bytes :: Text }
  deriving (Eq, Show, Read, Generic)

deriving newtype instance IsString Values

instance ToSchema Values where
  declareNamedSchema proxy = genericDeclareNamedSchema defaultSchemaOptions proxy
                             & mapped.schema.description ?~ "Content of the key-value store, an UTF-8 encoded string"
                             & mapped.schema.example ?~
                             toJSON (Values "Fifi" )


instance ToJSON Values where
  toJSON (Values bs) = object [ "data" .= bs ]

instance FromJSON Values where
  parseJSON = withObject "Values" $ \ o -> Values <$> o .: "data"

data Command = Create { value :: Values }
             | Modify { key :: Key, value :: Values }
             | ClearAll
  deriving (Eq, Show, Read, Generic, ToJSON, FromJSON)


data Event = Stored { storedKey :: Key, seed :: StdGen, value :: Values }
           | Cleared
           | Error { reason :: Text }
  deriving (Eq, Show, Read, Generic, ToJSON, FromJSON)

instance Eq StdGen where
  s == s' = show s == show s'

instance ToJSON StdGen where
  toJSON s = String $ Text.pack $ show s

instance FromJSON StdGen where
  parseJSON = withText "StdGen" $ pure . read . Text.unpack
