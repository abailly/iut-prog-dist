{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators    #-}

module KV where

import           Data.Default
import           Data.Functor                              (void)
import           KV.Server
import           KV.Store.DB
import           Network.Wai.Handler.Warp                  (run)
import           Network.Wai.Middleware.RequestLogger
import           Network.Wai.Middleware.RequestLogger.JSON

start :: Int -> IO ()
start port = do
  putStrLn $ "Starting K/V Store: " <> show port
  store <- makeStore
  logger <- doLog
  app <- server store
  void $ run port $ logger app
    where
      doLog = mkRequestLogger $ def { outputFormat = CustomOutputFormatWithDetails formatAsJSON }
