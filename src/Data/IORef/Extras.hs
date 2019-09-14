module Data.IORef.Extras
( module Data.IORef
, atomicModifyIORef_
, atomicModifyIORef_'
) where

import Data.IORef

atomicModifyIORef_ :: IORef a -> (a -> a) -> IO ()
atomicModifyIORef_ ref f = atomicModifyIORef ref $ \a -> (f a, ())

atomicModifyIORef_' :: IORef a -> (a -> a) -> IO ()
atomicModifyIORef_' ref f = atomicModifyIORef' ref $ \a -> (f a, ())