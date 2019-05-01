{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Monad.Observer where

import Control.Monad.Reader
import Data.Maybe

-- | There is no functional dependency @m -> a@ so it allows multiple uses of
-- MonadObserver to observer different things in the one function.
class Monad m => MonadObserver a m where
    askObserver :: m (a -> m ())

instance {-# OVERLAPPABLE #-} (Monad (t m), MonadTrans t, MonadObserver a m) => MonadObserver a (t m) where
    askObserver = lift $ (lift .) <$> askObserver

instance {-# OVERLAPPABLE #-} Monad m => MonadObserver a (ReaderT (a -> m ()) m) where
    askObserver = (lift .) <$> ask

observe :: MonadObserver a m => a -> m ()
observe a = askObserver >>= ($ a)

-- | There is a functional dependency @m -> a@ to prevent ambiguity in overlapping instances.
-- This means there can only be one observer at a time for each unique @f@.
class Monad m => MonadObserver' f a m | m -> a where
    askObserver' :: m (f a -> m ())

instance {-# OVERLAPPABLE #-} (Monad (t m), MonadTrans t, MonadObserver' f a m) => MonadObserver' f a (t m) where
    askObserver' = lift $ (lift .) <$> askObserver'

instance {-# OVERLAPPABLE #-} Monad m => MonadObserver' f a (ReaderT (f a -> m ()) m) where
    askObserver' = (lift .) <$> ask

observe' :: MonadObserver' f a m => f a -> m ()
observe' a = askObserver' >>= ($ a)
