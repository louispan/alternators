{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Monad.Observer where

import Control.Monad.Reader

-- | There is no functional dependency @m -> a@ so it allows multiple uses of
-- Observer to observer different things in the one function.
class Monad m => Observer a m where
    askObserver :: m (a -> m ())

instance {-# OVERLAPPABLE #-} (Monad (t m), MonadTrans t, Observer a m) => Observer a (t m) where
    askObserver = lift $ (lift .) <$> askObserver

instance {-# OVERLAPPABLE #-} Monad m => Observer a (ReaderT (a -> m ()) m) where
    askObserver = (lift .) <$> ask

observe :: Observer a m => a -> m ()
observe a = askObserver >>= ($ a)

-- | There is a functional dependency @m -> a@ to prevent ambiguity in overlapping instances.
-- This means there can only be one observer at a time for each unique @f@.
class Monad m => Observer' f a m | m -> a where
    askObserver' :: m (f a -> m ())

instance {-# OVERLAPPABLE #-} (Monad (t m), MonadTrans t, Observer' f a m) => Observer' f a (t m) where
    askObserver' = lift $ (lift .) <$> askObserver'

instance {-# OVERLAPPABLE #-} Monad m => Observer' f a (ReaderT (f a -> m ()) m) where
    askObserver' = (lift .) <$> ask

observe' :: Observer' f a m => f a -> m ()
observe' a = askObserver' >>= ($ a)

-- -- | 'runReaderT' that resolve ambiguity of @m@
-- runObserver' :: Monad m => ReaderT (f a -> m ()) m r -> (f a -> m ()) -> m r
-- runObserver' = runReaderT

-- | 'runReaderT' that resolve ambiguity of @m@
runObserver :: ReaderT (a -> m ()) m r -> (a -> m ()) -> m r
runObserver = runReaderT

-- | like 'fmap'ing the observed value
reobserve :: Observer b m => (a -> b) -> ReaderT (a -> m ()) m r -> m r
reobserve f m = runObserver m (observe . f)

-- | like 'fmap'ing the observed value
reobserve' :: Observer' f b m => (a -> f b) -> ReaderT (a -> m ()) m r -> m r
reobserve' f m = runObserver m (observe' . f)