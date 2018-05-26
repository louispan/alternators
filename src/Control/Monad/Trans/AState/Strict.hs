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

module Control.Monad.Trans.AState.Strict where

import Control.Applicative
import Control.Lens
import Control.Monad.Cont.Class
import Control.Monad.Error.Class
import Control.Monad.Fail
import Control.Monad.Fix
import Control.Monad.Morph
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Writer.Class
import Control.Newtype
import Data.Semigroup
import qualified GHC.Generics as G

-- | A newtype wrapper around StateT for lifted monoid instances.
-- Memonic: @A@ for alternative AStateT which can be merged into "a" single AStateT
newtype AStateT s m a = AStateT { unAStateT :: StateT s m a }
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

-- pattern AStateT' :: (s -> m (a, s)) -> AStateT s m a
-- pattern AStateT' f = AStateT (StateT f)

-- #if __GLASGOW_HASKELL__ >= 802
-- {-# COMPLETE AStateT' #-}
-- #endif

type AState s = AStateT s Identity

astateT :: (s -> m (a, s)) -> AStateT s m a
astateT = AStateT . StateT

astate :: (s -> (a, s)) -> AState s a
astate = AStateT . state

runAStateT :: AStateT s m a -> s -> m (a, s)
runAStateT = runStateT . unAStateT

runAState :: AState s a -> s -> (a, s)
runAState = runState . unAStateT

evalAStateT :: Monad m => AStateT s m a -> s -> m a
evalAStateT = evalStateT . unAStateT

evalAState :: AState s a -> s -> a
evalAState = evalState . unAStateT

execAStateT :: Monad m => AStateT s m a -> s -> m s
execAStateT = execStateT . unAStateT

execAState :: AState s a -> s -> s
execAState = execState . unAStateT

mapAStateT :: (m (a, s) -> n (b, s)) -> AStateT s m a -> AStateT s n b
mapAStateT f = AStateT . mapStateT f . unAStateT

mapAState :: ((a, s) -> (b, s)) -> AState s a -> AState s b
mapAState f = AStateT . mapState f . unAStateT

withAStateT :: (s -> s) -> AStateT s m a -> AStateT s m a
withAStateT f = AStateT . withStateT f . unAStateT

withAState :: (s -> s) -> AState s a -> AState s a
withAState = withAStateT

instance Newtype (AStateT s m a)

type instance Zoomed (AStateT s m) = Zoomed (StateT s m)
instance (Monad m) => Zoom (AStateT s m) (AStateT t m) s t where
    zoom l (AStateT f) = AStateT (zoom l f)

-- | This is the reason for the newtye wrapper.
-- Run both states, and (<>) the results.
instance (Semigroup (m a), Monad m) => Semigroup (AStateT s m a) where
    (AStateT (StateT f)) <> (AStateT (StateT g)) =
        AStateT . StateT $ \s -> do
            (a1, s1) <- f s
            (a2, s2) <- g s1
            (\a3 -> (a3, s2)) <$> ((pure a1) <> (pure a2))

-- | This is the reason for the newtye wrapper.
-- Run both states, and (`mappend`) the results.
instance (Monoid (m a), Monad m) => Monoid (AStateT s m a) where
    mempty = AStateT . StateT $ \s -> (\a -> (a, s)) <$> mempty
    (AStateT (StateT f)) `mappend` (AStateT (StateT g)) =
        AStateT . StateT $ \s -> do
            (a1, s1) <- f s
            (a2, s2) <- g s1
            (\a3 -> (a3, s2)) <$> ((pure a1) `mappend` (pure a2))
