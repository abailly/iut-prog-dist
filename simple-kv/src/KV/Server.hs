{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators    #-}
module KV.Server where

import           Control.Monad.Reader
import           KV.Store
import           KV.Types
import           Servant


type API = Capture "key" Key :> Get '[JSON] Bytes
  :<|> Capture "key" Key :> ReqBody '[JSON] Bytes :> Put '[JSON] NoContent
  :<|> ReqBody '[JSON] Bytes :> PostCreated '[JSON] Key

kvApi :: Proxy API
kvApi = Proxy

server :: (MonadStore s IO) => s -> IO Application
server store = pure $ serve kvApi $ hoistServer kvApi runServer prodHandler
  where
    runServer = Handler . flip runReaderT store
    prodHandler = handleGet :<|> handlePut :<|> handlePost

    handleGet k = do
      e <- ask >>= lift . send (Retrieve k)
      case e of
        Retrieved _ v -> pure v
        _             -> throwError err404

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
