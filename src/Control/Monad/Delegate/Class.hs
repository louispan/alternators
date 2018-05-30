{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Monad.Delegate.Class where

import Control.Monad.Trans.ACont
import Control.Monad.Trans.AReader
import Control.Monad.Trans.Class
import Control.Monad.Trans.Cont
import Control.Monad.Trans.Identity
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
import Data.Maybe

class Monad m => MonadDelegate r m | m -> r where
    delegate :: ((a -> m r) -> m r) -> m a

instance Monad m => MonadDelegate r (ContT r m) where
    delegate f = ContT $ \k -> evalContT $ f (lift . k)

instance Monad m => MonadDelegate r (AContT r m) where
    delegate f = acontT $ \k -> evalAContT $ f (lift . k)

instance (MonadDelegate r m) => MonadDelegate r (IdentityT m) where
    delegate f = IdentityT $ delegate $ \k -> runIdentityT $ f (lift . k)

instance (MonadDelegate r m) => MonadDelegate r (ReaderT env m) where
    delegate f = ReaderT $ \r -> delegate $ \k -> (`runReaderT` r) $ f (lift . k)

instance (MonadDelegate r m) => MonadDelegate r (AReaderT env m) where
    delegate f = areaderT $ \r -> delegate $ \k -> (`runAReaderT` r) $ f (lift . k)

instance (Monoid r, MonadDelegate r m) => MonadDelegate r (MaybeT m) where
    delegate f = MaybeT . fmap Just . delegate $ \k ->
        fmap (fromMaybe mempty) . runMaybeT . f $ lift . k

-- | Only handle with given monad.
finish :: forall a r m. MonadDelegate r m => m r -> m a
finish = delegate . const

-- | Swich the focus/result/event of MonadDelegate that fires two events.
retask :: MonadDelegate r m => ((b -> m r) -> m a) -> (a -> m r) -> m b
retask f ka = delegate $ \kb -> f kb >>= ka
