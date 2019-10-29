{-# LANGUAGE AllowAmbiguousTypes #-}
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
    -- , runObserverTagged
    , unobserve
    , unobserveTagged
    , unobserve2
    , unobserveTagged2
    , observeP
    , observe
    , observe'
    -- , observeTagged
    , reobserveP
    , reobserve
    , reobserve'
    , reobserveTagged
    ) where

import Control.Monad.Delegate
import Control.Monad.Environ
import Control.Monad.Reader
import Data.Proxy
import Data.Tagged.Extras

-- | There is no functional dependency @m -> a@ so it allows multiple uses of
-- Observer to observer different things in the one function.
-- The 'Proxy' @p@ allows controlled inference of @p m -> a@.
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

-- -- | Convenient version of 'runObserverT' for the case where event is @Tagged tag a@
-- -- but the tag is automatically unwrapped when used.
-- -- Use with @TypeApplications@ to specify @tag@
-- runObserverTagged :: forall tag a r m. ObserverT (Tagged tag a) m r -> (a -> m ()) -> m r
-- runObserverTagged m f = runReaderT m (f . untag' @tag)

-- | Convert from an @ObserverT a m ()@ to a 'MonadDelegate'
unobserve :: forall a m. MonadDelegate m => ObserverT a m () -> m a
unobserve m = delegate $ \fire -> runObserverT m fire

-- | Convenient version of 'unobserve' for the case where event is @Tagged tag a@
-- but the tag is automatically unwrapped when used.
unobserveTagged :: forall tag a m. MonadDelegate m => ObserverT (Tagged tag a) m () -> m a
unobserveTagged m = delegate $ \fire -> runObserverT m (fire . untag' @tag)

-- | Convert from an 'ObserverT a m ()' to a 'MonadDelegate', delegating both the @r@ and @a@
unobserve2 :: forall a r m. MonadDelegate m => ObserverT a m r -> m (Either r a)
unobserve2 m = delegate2 $ \(f, g) -> runObserverT m g >>= f

-- | Convenient version of 'unobserve2' for the case where event is @Tagged tag a@
-- but the tag is automatically unwrapped when used.
unobserveTagged2 :: forall tag a r m. MonadDelegate m => ObserverT (Tagged tag a) m r -> m (Either r a)
unobserveTagged2 m = delegate2 $ \(f, g) -> runObserverT @(Tagged tag a) m (g . untag' @tag) >>= f

-- | Fire an event @a@ where the 'MonadObserver' type ambiguity is resolved by the given 'Proxy' @p@
observeP :: forall p a m. MonadObserver p a m => Proxy p -> a -> m ()
observeP p a = askObserver p >>= ($ a)

-- | 'MonadObserver' where the proxy @p@ is specified with @TypeApplications'
observe :: forall p a m. MonadObserver p a m => a -> m ()
observe a = observeP @p Proxy a

-- | Convenient version of 'observe' for the case where the 'Proxy' @p@ and @a@ are the same
observe' :: forall a m. MonadObserver' a m => a -> m ()
observe' = observeP @a Proxy

-- -- | Convenient version of 'observe'' for the case where event is @Tagged tag a@
-- observeTagged :: forall tag a m. MonadObserver' (Tagged tag a) m => a -> m ()
-- observeTagged a = observe' (Tagged @tag a)

-- | like 'fmap'ing the observed value where the 'MonadObserver' type ambiguity is resolved by the given 'Proxy' @p@
reobserveP :: forall p a b r m. MonadObserver p b m => Proxy p -> (a -> b) -> ObserverT a m r -> m r
reobserveP p f m = runObserverT m (observeP p . f)

-- | 'reobserveP' where the proxy @p@ is specified with @TypeApplications'
reobserve :: forall p a b r m. MonadObserver p b m => (a -> b) -> ObserverT a m r -> m r
reobserve = reobserveP @p Proxy

-- | 'reobserveP' for the case where the 'Proxy' @p@ and @a@ are the same
reobserve' :: forall a b r m. MonadObserver' b m => (a -> b) -> ObserverT a m r -> m r
reobserve' = reobserveP @b Proxy

-- | 'reobserveP' for the case where event is @Tagged tag a@
-- but the tag is automatically unwrapped when used.
reobserveTagged :: forall tagA tagB a b r m. MonadObserver' (Tagged tagB b) m => (a -> b) -> ObserverT (Tagged tagA a) m r -> m r
reobserveTagged f = reobserve' @(Tagged tagA a) @(Tagged tagB b) (Tagged @tagB . f . untag' @tagA)




