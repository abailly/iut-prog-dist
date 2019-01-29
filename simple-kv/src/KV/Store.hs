{-# LANGUAGE FlexibleInstances     #-}
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

import           Control.Monad.Trans
import qualified Data.Map            as Map
import           KV.Types
import           System.Random

data Store = Store { values :: Map.Map Key Bytes
                   , seed   :: StdGen
                   }
  deriving (Eq,Show)

emptyStore :: StdGen -> Store
emptyStore = Store Map.empty

retrieve :: Key -> Store -> Maybe Bytes
retrieve k Store{values} = Map.lookup k values

list :: Store -> [ Bytes ]
list Store{values} = Map.elems values

act :: Command -> Store -> Event
act (Create d) Store{seed} =
  let (k, g) = random seed
  in Stored k g d
act (Modify k v) Store{seed} =
  Stored k seed v

apply :: Event -> Store -> Store
apply (Stored k s v) store@Store{values} = store { values = Map.insert k v values
                                                 , seed = s
                                                 }
apply _ store = store

actAndApply :: Command -> Store -> Store
actAndApply c s = apply (act c s) s

class MonadStore s m where
  send :: Command -> s -> m Event
  listStore :: s -> m [ Bytes ]
  retrieveStore :: Key -> s -> m (Maybe Bytes)

instance (MonadTrans t, Monad m, MonadStore s m) => MonadStore s (t m) where
  retrieveStore k s = lift $ retrieveStore k s
  listStore s = lift $ listStore s
  send c s = lift $ send c s
