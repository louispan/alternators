{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- #if __GLASGOW_HASKELL__ < 802
-- {-# OPTIONS_GHC -Wno-incomplete-patterns #-}
-- #endif

module Control.Monad.Trans.States.Lazy where

import Control.Applicative
import Control.Lens
import Control.Monad.Cont.Class
import Control.Monad.Error.Class
import Control.Monad.Fail
import Control.Monad.Fix
import Control.Monad.Morph
import Control.Monad.Reader
import Control.Monad.State.Lazy
import Control.Monad.Writer.Class
import Control.Newtype
import Data.Semigroup
import qualified GHC.Generics as G

-- | A newtype wrapper around StateT for lifted monoid instances.
-- Memonic: the @s@ means plural, alluding to the monoidal property.
newtype StatesT s m a = StatesT { unStatesT :: StateT s m a }
    deriving
    ( G.Generic
    , MonadTrans
    , Monad
    , Functor
    , MonadFix
    , MonadFail
    , Applicative
    , MonadIO
    , Alternative
    , MonadPlus
    , MonadReader r
    , MonadState s
    , MonadWriter w
    , MonadError e
    , MFunctor
    , MonadCont
    )

-- pattern StatesT' :: (s -> m (a, s)) -> StatesT s m a
-- pattern StatesT' f = StatesT (StateT f)

-- #if __GLASGOW_HASKELL__ >= 802
-- {-# COMPLETE StatesT' #-}
-- #endif

type States s  = StatesT s Identity

statesT :: (s -> m (a, s)) -> StatesT s m a
statesT = StatesT . StateT

states :: (s -> (a, s)) -> States s a
states = StatesT . state

runStatesT :: StatesT s m a -> s -> m (a, s)
runStatesT = runStateT . unStatesT

runStates :: States s a -> s -> (a, s)
runStates = runState . unStatesT

evalStatesT :: Monad m => StatesT s m a -> s -> m a
evalStatesT = evalStateT . unStatesT

evalStates :: States s a -> s -> a
evalStates = evalState . unStatesT

execStatesT :: Monad m => StatesT s m a -> s -> m s
execStatesT = execStateT . unStatesT

execStates :: States s a -> s -> s
execStates = execState . unStatesT

mapStatesT :: (m (a, s) -> n (b, s)) -> StatesT s m a -> StatesT s n b
mapStatesT f = StatesT . mapStateT f . unStatesT

mapStates :: ((a, s) -> (b, s)) -> States s a -> States s b
mapStates f = StatesT . mapState f . unStatesT

withStatesT :: (s -> s) -> StatesT s m a -> StatesT s m a
withStatesT f = StatesT . withStateT f . unStatesT

withStates :: (s -> s) -> States s a -> States s a
withStates = withStatesT

instance Newtype (StatesT s m a)

type instance Zoomed (StatesT s m) = Zoomed (StateT s m)
instance (Monad m) => Zoom (StatesT s m) (StatesT t m) s t where
    zoom l (StatesT f) = StatesT (zoom l f)

-- | This is the reason for the newtye wrapper
-- This is different from the Alternative/MonadPlus instance.
-- The Alternative/MonadPlus instance runs one or the other
-- The Semigroup/Monoid instance runs both.
-- This Semigroup instance is the same as @(->) r@
instance (Semigroup a, Monad m) => Semigroup (StatesT s m a) where
    (StatesT f) <> (StatesT g) = StatesT (liftA2 (<>) f g)

-- | This is the reason for the newtye wrapper
-- This is different from the Alternative/MonadPlus instance.
-- The Alternative/MonadPlus instance runs one or the other
-- The Semigroup/Monoid instances runs both.
-- This Monoid instance is the same as @(->) r@
instance (Monoid a, Monad m) => Monoid (StatesT s m a) where
    mempty = StatesT (pure mempty)
    (StatesT f) `mappend` (StatesT g) = StatesT (liftA2 mappend f g)
