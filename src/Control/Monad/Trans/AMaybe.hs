{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
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

module Control.Monad.Trans.AMaybe where

import Control.Applicative
import Control.Lens
import Control.Monad.Cont.Class
import Control.Monad.Error.Class
import Control.Monad.Fail
import Control.Monad.Fix
import Control.Monad.Morph
import Control.Monad.Reader
import Control.Monad.State.Class
import Control.Monad.Trans.Maybe
import Control.Monad.Writer.Class
import Control.Monad.Zip
import Control.Newtype
import Data.Functor.Classes
import Data.Semigroup
import qualified GHC.Generics as G

-- | A newtype wrapper around MaybeT for lifted monoid instances.
-- Memonic: @A@ for alternative MaybeT which can be merged into "a" single MaybeT
newtype AMaybeT m a = AMaybeT { unAMaybeT :: MaybeT m a }
    deriving
    ( G.Generic
    , MonadTrans
    , Monad
    , Functor
    , Foldable
    , Traversable
    , Eq1
    , Ord1
    , Read1
    , Show1
    , Eq
    , Ord
    , Read
    , Show
    , MonadFix
    , MonadFail
    , Applicative
    , MonadZip
    , MonadIO
    , Alternative
    , MonadPlus
    , MonadReader r
    , MonadState s
    , MonadWriter w
    , MonadError e
    , MonadCont
    , MFunctor
    , MMonad
    )

-- pattern AMaybeT' :: (r -> m a) -> AMaybeT r m a
-- pattern AMaybeT' f = AMaybeT (ReaderT f)

-- #if __GLASGOW_HASKELL__ >= 802
-- {-# COMPLETE AMaybeT' #-}
-- #endif

amaybeT :: m (Maybe a) -> AMaybeT m a
amaybeT = AMaybeT . MaybeT

runAMaybeT :: AMaybeT m a -> m (Maybe a)
runAMaybeT = runMaybeT . unAMaybeT

mapAMaybeT :: (m (Maybe a) -> n (Maybe b)) -> AMaybeT m a -> AMaybeT n b
mapAMaybeT f = AMaybeT . mapMaybeT f . unAMaybeT

instance Newtype (AMaybeT m a)

type instance Zoomed (AMaybeT m) = Zoomed (MaybeT m)
instance Zoom m n s t => Zoom (AMaybeT m) (AMaybeT n) s t where
    zoom l (AMaybeT f) = AMaybeT (zoom l f)

-- | This is the reason for the newtye wrapper
instance (Semigroup (m (Maybe a))) => Semigroup (AMaybeT m a) where
    (AMaybeT (MaybeT f)) <> (AMaybeT (MaybeT g)) = AMaybeT . MaybeT $ f <> g

-- | This is the reason for the newtye wrapper
instance (Monoid (m (Maybe a))) => Monoid (AMaybeT m a) where
    mempty = AMaybeT (MaybeT mempty)
    (AMaybeT (MaybeT f)) `mappend` (AMaybeT (MaybeT g)) = AMaybeT . MaybeT $ f `mappend` g
