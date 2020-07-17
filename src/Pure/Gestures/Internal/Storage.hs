{-# language CPP #-}
module Pure.Gestures.Internal.Storage 
  ( store
  , retrieve
  , remove 
  ) where

import Pure.Data.JSON
import Pure.Data.Txt
import Control.Monad
import Data.IORef
import Data.Map as Map
import System.IO.Unsafe
import Prelude hiding (read)

{-# NOINLINE internal_store #-}
internal_store :: IORef (Map Txt Value)
internal_store = unsafePerformIO (newIORef mempty)

store :: ToJSON a => Txt -> a -> IO ()
store k v = do
  atomicModifyIORef' internal_store $ \st ->
   (Map.insert k (toJSON v) st,())

retrieve :: FromJSON a => Txt -> IO (Maybe a)
retrieve k = do
  st <- readIORef internal_store
  case Map.lookup k st of
    Just v | Success a <- fromJSON v -> pure (Just a)
    _ -> pure Nothing

remove :: Txt -> IO ()
remove k =
  atomicModifyIORef' internal_store $ \st ->
    (Map.delete k st,())