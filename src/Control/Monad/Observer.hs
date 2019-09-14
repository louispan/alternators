{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Monad.Observer where

import Control.Monad.Reader

-- | There is no functional dependency @m -> a@ so it allows multiple uses of
-- Observer to observer different things in the one function.
-- Use newtype wrappers or 'Tagged' to ensure unique @a@ in the monad stack.
-- The main instance of 'Observer' is the 'ReaderT' instance which holds a function
-- @a -> m ()@, ie not @a -> ReaderT m ()@ otherwise there will be a @ReaderT m ~ m@ compile error.
-- This class is required because using 'Control.Monad.Context.MonadAsk' to emulate
-- the same signature will result in instance resolution ambiguities
-- since GHC doesn't know that the @m@ in the @(a -> m())@ is the same as the monad @m@.
class Monad m => MonadObserver a m where
    askObserver :: m (a -> m ())

instance {-# OVERLAPPABLE #-} (Monad (t m), MonadTrans t, MonadObserver a m) => MonadObserver a (t m) where
    askObserver = lift $ (lift .) <$> askObserver

instance {-# OVERLAPPABLE #-} Monad m => MonadObserver a (ReaderT (a -> m ()) m) where
    askObserver = (lift .) <$> ask

type ObserverT a m = ReaderT (a -> m ()) m

observe :: MonadObserver a m => a -> m ()
observe a = askObserver >>= ($ a)

-- | 'runReaderT' that resolve ambiguity of @m@
runObserverT :: ObserverT a m r -> (a -> m ()) -> m r
runObserverT = runReaderT

-- | like 'fmap'ing the observed value
reobserve :: MonadObserver b m => (a -> b) -> ObserverT a m r -> m r
reobserve f m = runObserverT m (observe . f)
