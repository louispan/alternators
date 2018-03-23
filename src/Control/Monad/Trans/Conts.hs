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

module Control.Monad.Trans.Conts where

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
-- Memonic: the @s@ means plural, alluding to the monoidal property.
newtype ContsT r m a = ContsT { unContsT :: ContT r m a }
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

type Conts r = ContsT r Identity

-- pattern ContsT' :: ((a -> m ()) -> m ()) -> ContsT m a
-- pattern ContsT' f = ContsT (ContT f)

-- #if __GLASGOW_HASKELL__ >= 802
-- {-# COMPLETE ContsT' #-}
-- #endif

contsT :: ((a -> m r) -> m r) -> ContsT r m a
contsT = ContsT . ContT

conts :: ((a -> r) -> r) -> Conts r a
conts = ContsT . cont

runContsT :: ContsT r m a -> (a -> m r) -> m r
runContsT = runContT . unContsT

runConts :: Conts r a -> (a -> r) -> r
runConts = runCont . unContsT

evalContsT :: Monad m => ContsT r m r -> m r
evalContsT = evalContT . unContsT

evalConts :: Conts r r -> r
evalConts = evalCont . unContsT

mapContsT :: (m r -> m r) -> ContsT r m a -> ContsT r m a
mapContsT f = ContsT . mapContT f . unContsT

withContsT :: ((b -> m r) -> a -> m r) -> ContsT r m a -> ContsT r m b
withContsT f = ContsT . withContT f . unContsT

withConts :: ((b -> r) -> a -> r) -> Conts r a -> Conts r b
withConts f = ContsT . withCont f . unContsT

instance Newtype (ContsT r m a)

deriving instance MonadState s (ContT r m) => MonadState s (ContsT r m)

instance MonadZip (ContsT r m) where
    mzip x y = liftA2 (,) x y

-- | This is the reason for the newtye wrapper
-- This is different from the Alternative/MonadPlus instance.
-- The Alternative/MonadPlus instance runs one or the other
-- The Semigroup/Monoid instances runs both, and fires the output twice.
instance (Semigroup r, Applicative m) => Semigroup (ContsT r m c) where
    ContsT (ContT f) <> ContsT (ContT g) =
        ContsT . ContT $ \k -> liftA2 (<>) (f k) (g k)

-- | This is the reason for the newtye wrapper
-- This is different from the Alternative/MonadPlus instance.
-- The Alternative/MonadPlus instance runs one or the other
-- The Semigroup/Monoid instances runs both, and fires the output twice.
instance (Monoid r, Applicative m) => Monoid (ContsT r m a) where
    mempty = ContsT . ContT . const $ pure mempty
    ContsT (ContT f) `mappend` ContsT (ContT g) =
        ContsT . ContT $ \k -> liftA2 mappend (f k) (g k)

-- | ContT didn't have an instance of Alternative1
instance Alternative m => Alternative (ContsT r m) where
    empty = ContsT $ ContT $ const empty
    ContsT (ContT f) <|> ContsT (ContT g) =
        ContsT $ ContT $ \k -> f k <|> g k

instance MonadPlus m => MonadPlus (ContsT r m) where
    mzero = empty
    mplus = (<|>)
