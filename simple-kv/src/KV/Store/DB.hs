{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
module KV.Store.DB where

import           Control.Concurrent.MVar
import           Control.Exception
import           Control.Monad.Trans     (MonadIO (..))
import           Data.Aeson              (eitherDecode, encode)
import qualified Data.Map                as Map
import           Data.Text.Lazy.Encoding (decodeUtf8, encodeUtf8)
import           Data.Text.Lazy.IO       (hGetLine, hPutStrLn)
import           KV.Log                  (withinLog)
import           KV.Store
import qualified System.IO               as IO
import           System.IO.Error
import           System.Random

data StoreDB = StoreDB { storeRef  :: MVar Store
                       , eventSink :: IO.Handle
                       }

makeStore :: IO StoreDB
makeStore = do
  h <- IO.openFile "store.db" IO.ReadWriteMode
  g <- newStdGen
  IO.hSetBuffering h IO.NoBuffering
  ref <- catchup h (emptyStore g) >>= newMVar
  pure $ StoreDB ref h

instance MonadStore StoreDB IO where

  listStore StoreDB{storeRef} = list <$> readMVar storeRef

  retrieveStore k StoreDB{storeRef} = retrieve k <$> readMVar storeRef

  send input StoreDB{storeRef, eventSink} = withinLog input $ liftIO $ modifyMVar storeRef $ \ store@Store{..} -> do
    let event = act input store
    hPutStrLn eventSink (decodeUtf8 $ encode event)
    IO.hFlush eventSink
    pure (apply event store, event)

resetStore :: (MonadIO m) => StoreDB -> m ()
resetStore StoreDB{storeRef,eventSink} = liftIO $ modifyMVar_ storeRef $ \ Store{..} -> do
  g <- newStdGen
  IO.hSeek eventSink IO.AbsoluteSeek 0
  IO.hSetFileSize eventSink 0
  pure $ Store Map.empty g

catchup :: IO.Handle -> Store -> IO Store
catchup eventSink store =
  (readAndApplyOneEvent store eventSink >>= catchup eventSink)
  `catch` \ (e :: IOException) -> if isEOFError e
                                  then pure store
                                  else throwIO e

readAndApplyOneEvent :: Store -> IO.Handle -> IO Store
readAndApplyOneEvent store eventSink = do
  serializedEvent <- hGetLine eventSink
  either
    (throwIO . userError . (\ e -> "failed to decode event " <> show serializedEvent <> ": " <> e))
    (pure . flip apply store)
    $ eitherDecode (encodeUtf8 serializedEvent)
