{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Monad.Defer.Class where

import Control.Monad.Trans.ACont
import Control.Monad.Trans.AReader
import Control.Monad.Trans.Class
import Control.Monad.Trans.Cont
import Control.Monad.Trans.Identity
import Control.Monad.Trans.Reader

class Monad m => MonadDefer r m | m -> r where
    defer :: ((a -> m r) -> m r) -> m a

instance Monad m => MonadDefer r (ContT r m) where
    defer f = ContT $ \k -> evalContT $ f (lift . k)

instance Monad m => MonadDefer r (AContT r m) where
    defer f = acontT $ \k -> evalAContT $ f (lift . k)

instance (MonadDefer r m) => MonadDefer r (IdentityT m) where
    defer f = IdentityT $ defer $ \k -> runIdentityT $ f (lift . k)

instance (MonadDefer r m) => MonadDefer r (ReaderT env m) where
    defer f = ReaderT $ \r -> defer $ \k -> (`runReaderT` r) $ f (lift . k)

instance (MonadDefer r m) => MonadDefer r (AReaderT env m) where
    defer f = areaderT $ \r -> defer $ \k -> (`runAReaderT` r) $ f (lift . k)
