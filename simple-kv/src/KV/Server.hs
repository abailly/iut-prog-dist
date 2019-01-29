{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators    #-}
module KV.Server where

import           Control.Lens
import           Control.Monad.Reader
import           Data.Swagger
import           KV.Store
import           KV.Types
import           Servant
import           Servant.Swagger


type API = Get '[JSON] [Values]
  :<|> Capture "key" Key :> Get '[JSON] Values
  :<|> Capture "key" Key :> ReqBody '[JSON] Values :> Put '[JSON] NoContent
  :<|> ReqBody '[JSON] Values :> PostCreated '[JSON] Key

kvApi :: Proxy API
kvApi = Proxy

type SwaggerEndpoint = "swagger.json" :> Get '[JSON] Swagger

fullApi :: Proxy (SwaggerEndpoint :<|> API)
fullApi = Proxy

-- | Swagger spec for Todo API.
kvSwagger :: Swagger
kvSwagger = toSwagger kvApi
            & info.title   .~ "Key/Value Store API"
            & info.version .~ "1.0"
            & info.description ?~ "An API for a dumb Key-Value store"
            & info.license ?~ ("MIT" & url ?~ URL "http://mit.com")

server :: (MonadStore s IO) => s -> IO Application
server store = pure $ serve fullApi $ hoistServer fullApi runServer prodHandler
  where
    runServer = Handler . flip runReaderT store
    prodHandler = pure kvSwagger :<|> (handleList :<|> handleGet :<|> handlePut :<|> handlePost)

    handleList = lift . listStore =<< ask

    handleGet k = do
      e <- (lift . retrieveStore k) =<< ask
      case e of
        Just v  -> pure v
        Nothing -> throwError err404

    handlePut k v = do
      e <- ask >>= lift . send (Modify k v)
      case e of
        Stored _ _ _ -> pure NoContent
        _            -> throwError err500

    handlePost bs = do
      e <- ask >>= lift . send (Create bs)
      case e of
        Stored k _ _ -> pure k
        _            -> throwError err500
