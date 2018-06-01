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
    -- | The inverse of 'delegate' is 'bind'
    --
    -- @
    -- (>>=) :: Monad m => m a -> (a -> m r) -> m r
    -- @
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

-- | Only handle with given monad, and ignore anything else.
-- This means subseqent fmap, aps, binds are always ignored.
-- @forall@ so @TypeApplications@ can be used to specify the type of @a@
finish :: forall a r m. MonadDelegate r m => m r -> m a
finish = delegate . const

-- | Convert two handler to a monad that may fire two possibilities
-- | The inverse is 'bind2'.
multitask :: MonadDelegate r m => ((a -> m r) -> (b -> m r) -> m r) -> m (Either a b)
multitask g = delegate $ \fab -> g (fab . Left) (fab . Right)

-- | Convert a monad that fires two possibilites to a two handlers.
bind2 :: Monad m => m (Either a b) -> (a -> m r) -> (b -> m r) -> m r
bind2 m fa fb = m >>= either fa fb

-- | 'bind' only the 'Right' possibility.
bindRight :: Monad m => m (Either a b) -> (b -> m c) -> m (Either a c)
bindRight m k = bind2 m (pure . Left) (fmap Right . k)

-- | 'bind' only the 'Left' possibility.
bindLeft :: Monad m => m (Either a b) -> (a -> m c) -> m (Either c b)
bindLeft m k = bind2 m (fmap Left . k) (pure . Right)

-- | finish the 'Left' possibility
finishLeft :: MonadDelegate r m => m (Either r b) -> m b
finishLeft m = m >>= either (finish . pure) pure

-- | finish the 'Right' possibility
finishRight :: MonadDelegate r m => m (Either a r) -> m a
finishRight m = m >>= either pure (finish . pure)
