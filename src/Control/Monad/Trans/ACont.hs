{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- #if __GLASGOW_HASKELL__ < 802
-- {-# OPTIONS_GHC -Wno-incomplete-patterns #-}
-- #endif

module Control.Monad.Trans.ACont where

import Control.Applicative
import Control.Monad.Cont
import Control.Monad.Fail
import Control.Monad.Reader
import Control.Monad.State.Class
import Control.Monad.Trans.Cont
import Control.Monad.Zip
import Control.Newtype
import Data.Functor.Identity
import Data.Semigroup
import qualified GHC.Generics as G

-- | A newtype wrapper around ContT for lifted monoid instances.
-- Memonic: @A@ for alternative AContT which can be merged into "a" single AContT
newtype AContT r m a = AContT { unAContT :: ContT r m a }
    deriving
    ( G.Generic
    , MonadTrans
    , Monad
    , Functor
    , MonadFail
    , Applicative
    , MonadIO
    , MonadReader r
    , MonadCont
    )

type ACont r = AContT r Identity

-- pattern AContT' :: ((a -> m ()) -> m ()) -> AContT m a
-- pattern AContT' f = AContT (ContT f)

-- #if __GLASGOW_HASKELL__ >= 802
-- {-# COMPLETE AContT' #-}
-- #endif

acontT :: ((a -> m r) -> m r) -> AContT r m a
acontT = AContT . ContT

acont :: ((a -> r) -> r) -> ACont r a
acont = AContT . cont

runAContT :: AContT r m a -> (a -> m r) -> m r
runAContT = runContT . unAContT

runACont :: ACont r a -> (a -> r) -> r
runACont = runCont . unAContT

evalAContT :: Monad m => AContT r m r -> m r
evalAContT = evalContT . unAContT

evalACont :: ACont r r -> r
evalACont = evalCont . unAContT

mapAContT :: (m r -> m r) -> AContT r m a -> AContT r m a
mapAContT f = AContT . mapContT f . unAContT

withAContT :: ((b -> m r) -> a -> m r) -> AContT r m a -> AContT r m b
withAContT f = AContT . withContT f . unAContT

withACont :: ((b -> r) -> a -> r) -> ACont r a -> ACont r b
withACont f = AContT . withCont f . unAContT

instance Newtype (AContT r m a)

deriving instance MonadState s (ContT r m) => MonadState s (AContT r m)

instance MonadZip (AContT r m) where
    mzip x y = liftA2 (,) x y

-- | This is the reason for the newtye wrapper
-- This is different from the Alternative/MonadPlus instance.
-- The Alternative/MonadPlus instance runs one or the other
-- The Semigroup/Monoid instances runs both, and fires the output twice.
instance (Semigroup r, Applicative m) => Semigroup (AContT r m c) where
    AContT (ContT f) <> AContT (ContT g) =
        AContT . ContT $ \k -> liftA2 (<>) (f k) (g k)

-- | This is the reason for the newtye wrapper
-- This is different from the Alternative/MonadPlus instance.
-- The Alternative/MonadPlus instance runs one or the other
-- The Semigroup/Monoid instances runs both, and fires the output twice.
instance (Monoid r, Applicative m) => Monoid (AContT r m a) where
    mempty = AContT . ContT . const $ pure mempty
    AContT (ContT f) `mappend` AContT (ContT g) =
        AContT . ContT $ \k -> liftA2 mappend (f k) (g k)

-- | This is the reason for the newtye wrapper
-- ContT didn't have an instance of Alternative
instance Alternative m => Alternative (AContT r m) where
    empty = AContT $ ContT $ const empty
    AContT (ContT f) <|> AContT (ContT g) =
        AContT $ ContT $ \k -> f k <|> g k

instance MonadPlus m => MonadPlus (AContT r m) where
    mzero = empty
    mplus = (<|>)
