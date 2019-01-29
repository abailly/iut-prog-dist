{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
module KV.Store.Mem where

import           Control.Concurrent.MVar
import           KV.Store
import           System.Random

data StoreDB = StoreDB { storeRef  :: MVar Store }

makeStore :: StdGen -> IO StoreDB
makeStore g = StoreDB <$> newMVar (emptyStore g)

instance MonadStore StoreDB IO where

  listStore StoreDB{storeRef} = list <$> readMVar storeRef

  retrieveStore k StoreDB{storeRef} = retrieve k <$> readMVar storeRef

  send input StoreDB{storeRef} = modifyMVar storeRef $ \ store@Store{..} -> do
    let event = act input store
    pure (apply event store, event)
