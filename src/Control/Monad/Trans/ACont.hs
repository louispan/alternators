{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}

module Control.Monad.Trans.ACont where

import Control.Applicative
import Control.Monad.Cont
import Control.Monad.Delegate
import Control.Monad.Fail

-- | 'AContT' is a 'ContT' with an 'Alternative' instance.
-- This means @AContT () MaybeT m@ is an instance of 'Alternative', 'MonadDelegate', and 'MonadDischarge'.
--
-- There are at least two possible instances of Alternative for ContT.
-- https://hub.darcs.net/ross/transformers/issue/66
-- https://hub.darcs.net/ross/transformers/issue/69
-- 'AContT' uses the lifted instance https://hub.darcs.net/ross/transformers/issue/66
-- which matches the instance in Control.Monad.Codensity.
newtype AContT r m a = AContT { runAContT :: (a -> m r) -> m r }
    deriving
        ( Functor, Applicative
        , Monad
        , MonadIO
        , MonadCont
        , MonadFail
        ) via (ContT r m)

deriving via (ContT () m) instance Monad m => MonadDelegate (AContT () m)
deriving via (ContT () m) instance Monad m => MonadDischarge (AContT () m)

instance MonadTrans (AContT r) where
    lift m = AContT (m >>=)

instance Alternative m => Alternative (AContT r m) where
    empty = AContT $ \_ -> empty
    (AContT f) <|> (AContT g) = AContT $ \k -> f k <|> g k

#if __GLASGOW_HASKELL__ >= 710
instance Alternative m => MonadPlus (AContT r m)
#else
instance MonadPlus m => MonadPlus (AContT r m) where
    mzero = AContT $ \_ -> mzero
    (AContT f) `mplus` (AContT g) = AContT $ \k -> f k `mplus` g k
#endif

-- | 'evalContT' for 'AContT'
evalAContT :: (Applicative m) => AContT r m r -> m r
evalAContT m = runAContT m pure

-- | 'withContT' for 'AContT'
withAContT :: ((b -> m r) -> (a -> m r)) -> AContT r m a -> AContT r m b
withAContT f m = AContT $ runAContT m . f

-- | @'liftLocal' ask local@ yields a @local@ function for @'ContT' r m@.
liftLocal :: (Monad m) => m r' -> ((r' -> r') -> m r -> m r) ->
    (r' -> r') -> AContT r m a -> AContT r m a
liftLocal ask local f m = AContT $ \ c -> do
    r <- ask
    local f (runAContT m (local (const r) . c))
