{-# LANGUAGE FlexibleInstances #-}

module Control.Monad.Delegate.Class where

import Control.Monad.Trans
import Control.Monad.Trans.Cont
import Control.Monad.Trans.Delegate
import Control.Monad.Trans.Identity
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Readers

class Monad m => MonadDelegate m where
    -- | Wrap an action that uses a handler for @a@ to result in @m ()@
    -- into an action that fires @a@
    delegating :: ((a -> m ()) -> m ()) -> m a

instance Monad m => MonadDelegate (ContT () m) where
    delegating f = ContT $ \k -> runContT (f (lift <$> k)) pure

instance Monad m => MonadDelegate (DelegateT m) where
    delegating f = delegateT' $ \k -> runDelegateT' (f (lift <$> k)) pure

instance MonadDelegate m => MonadDelegate (IdentityT m) where
    delegating f = IdentityT $ delegating $ \k -> runIdentityT (f (lift <$> k))

instance MonadDelegate m => MonadDelegate (ReaderT r m) where
    delegating f = ReaderT $ \r -> delegating $ \k -> runReaderT (f (lift <$> k)) r

instance MonadDelegate m => MonadDelegate (ReadersT r m) where
    delegating f = readersT' $ \r -> delegating $ \k -> runReadersT' (f (lift <$> k)) r
