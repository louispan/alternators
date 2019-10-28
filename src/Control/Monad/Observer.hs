{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Monad.Observer
    ( module Data.Proxy
    , MonadObserver(..)
    , MonadObserver'
    , ObserverT
    , runObserverT
    , unobserve
    , unobserve2
    , observe
    , observe'
    , reobserve
    , reobserve'
    ) where

import Control.Monad.Delegate
import Control.Monad.Environ
import Control.Monad.Reader
import Data.Proxy

-- | There is no functional dependency @m -> a@ so it allows multiple uses of
-- Observer to observer different things in the one function.
-- The 'Proxy' @p@ allows controlled inference of @p m -> s@.
-- The main instance of 'Observer' is the 'ReaderT' instance which holds a function
-- @a -> m ()@, ie not @a -> ReaderT m ()@ otherwise there will be a @ReaderT m ~ m@ compile error.
class Monad m => MonadObserver p a m | p m -> a where
    askObserver :: Proxy p -> m (a -> m ())

type MonadObserver' a = MonadObserver a a

instance {-# OVERLAPPABLE #-} (Monad (t m), MonadTrans t, MonadObserver p a m) => MonadObserver p a (t m) where
    askObserver p = lift $ (lift .) <$> askObserver p

instance {-# OVERLAPPABLE #-} Monad m => MonadObserver p a (ReaderT (a -> m ()) m) where
    askObserver _ = (lift .) <$> ask

type ObserverT a m = ReaderT (a -> m ()) m

-- | 'runReaderT' that resolve ambiguity of @m@
runObserverT :: forall a r m. ObserverT a m r -> (a -> m ()) -> m r
runObserverT = runReaderT

unobserve :: forall a m. MonadDelegate m => ObserverT a m () -> m a
unobserve m = delegate $ \fire -> runObserverT m fire

unobserve2 :: forall a r m. MonadDelegate m => ObserverT a m r -> m (Either r a)
unobserve2 m = delegate2 $ \(f, g) -> runObserverT m g >>= f

observe :: forall p a m. MonadObserver p a m => Proxy p -> a -> m ()
observe p a = askObserver p >>= ($ a)

observe' :: forall a m. MonadObserver' a m => a -> m ()
observe' = observe @a Proxy

-- | like 'fmap'ing the observed value
reobserve :: forall p a b r m. MonadObserver p b m => Proxy p -> (a -> b) -> ObserverT a m r -> m r
reobserve p f m = runObserverT m (observe p . f)

reobserve' :: forall a b r m. MonadObserver' b m => (a -> b) -> ObserverT a m r -> m r
reobserve' = reobserve @b Proxy
