{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
-- | A backend store based on event sourcing
--
--  * Events affecting store are persisted as a stream of events into a file
--  * State is cached in memory
--
module KV.Store where

import           Control.Concurrent.MVar
import           Control.Exception
import           Control.Monad.Trans     (MonadIO (..))
import           Data.Aeson              (eitherDecode, encode)
import           Data.ByteString         (ByteString)
import qualified Data.Map                as Map
import           Data.Text.Lazy.Encoding (decodeUtf8, encodeUtf8)
import           Data.Text.Lazy.IO       (hGetLine, hPutStrLn)
import           KV.Log                  (withinLog)
import           KV.Types
import qualified System.IO               as IO
import           System.IO.Error

data Store = Store { values    :: Map.Map Key ByteString
                   , eventSink :: IO.Handle
                   }

send :: (MonadIO m) => Command -> StoreDB -> m Event
send input storedb = withinLog input $ liftIO $ modifyMVar storedb $ \ store@Store{..} -> do
  let event = act input store
  hPutStrLn eventSink (decodeUtf8 $ encode event)
  IO.hFlush eventSink
  pure (apply event store, event)

resetStore :: (MonadIO m) => StoreDB -> m ()
resetStore db = liftIO $ modifyMVar_  db $ \ Store{..} -> do
  IO.hSeek eventSink IO.AbsoluteSeek 0
  IO.hSetFileSize eventSink 0
  pure $ Store Map.empty eventSink

act :: Command -> Store -> Event
act _  _ = Error { reason = "Not Implemented" }

apply :: Event -> Store -> Store
apply _ store       = store

type StoreDB = MVar Store

makeStore :: IO StoreDB
makeStore = do
  h <- IO.openFile "store.db" IO.ReadWriteMode
  IO.hSetBuffering h IO.NoBuffering
  let store = Store Map.empty h
  catchup store >>= newMVar

catchup :: Store -> IO Store
catchup store =
  (readAndApplyOneEvent store >>= catchup)
  `catch` \ (e :: IOException) -> if isEOFError e
                                  then pure store
                                  else throwIO e

readAndApplyOneEvent :: Store -> IO Store
readAndApplyOneEvent store@Store{eventSink} = do
  serializedEvent <- hGetLine eventSink
  either
    (throwIO . userError . (\ e -> "failed to decode event " <> show serializedEvent <> ": " <> e))
    (pure . flip apply store)
    $ eitherDecode (encodeUtf8 serializedEvent)
