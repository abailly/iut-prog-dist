{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators    #-}

module KV where

import           Control.Monad.Reader
import           Data.Default
import           Data.Functor                              (void)
import           KV.Store
import           KV.Types
import           Network.Wai.Handler.Warp                  (run)
import           Network.Wai.Middleware.RequestLogger
import           Network.Wai.Middleware.RequestLogger.JSON
import           Servant

type API = Capture "key" Key :> Get '[JSON] Bytes
  :<|> Capture "key" Key :> ReqBody '[JSON] Bytes :> Put '[JSON] NoContent
  :<|> ReqBody '[JSON] Bytes :> Post '[JSON] Key

kvApi :: Proxy API
kvApi = Proxy

start :: Int -> IO ()
start port = do
  putStrLn $ "Starting K/V Store: " <> show port
  store <- makeStore
  logger <- doLog
  void $ run port $ logger $ server store
    where
      doLog = mkRequestLogger $ def { outputFormat = CustomOutputFormatWithDetails formatAsJSON }

      runServer store = Handler . flip runReaderT store

      server store = serve kvApi $ hoistServer kvApi (runServer store) prodHandler

      prodHandler = undefined
