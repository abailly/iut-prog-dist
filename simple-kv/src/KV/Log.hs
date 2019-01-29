{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}
module KV.Log where

import           Control.Monad.Trans
import           Data.Aeson
import           Data.ByteString.Lazy.Char8 as IO
import           Data.Time.Clock.System
import           Data.Time.Format
import           Servant                    (NoContent)

class (Monad m) => MonadLog m where
  mlog :: (ToJSON a) => a -> m ()

  withinLog :: (ToJSON a, ToJSON b) => a -> m b -> m b
  withinLog start act = do
    mlog start
    res <- act
    mlog res
    pure res

instance ToJSON NoContent where
  toJSON _ = Null

instance (Monad m, MonadIO m) => MonadLog m where
  mlog a = liftIO $ do
    ts <- systemToUTCTime <$> getSystemTime
    IO.putStrLn $ encode $ object [ "timestamp" .= formatTime defaultTimeLocale (iso8601DateFormat (Just "%H:%M:%S%Q")) ts
                                  , "message" .= a
                                  ]
