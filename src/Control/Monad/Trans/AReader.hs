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

module Control.Monad.Trans.AReader where

import Control.Applicative
import Control.Lens
import Control.Monad.Cont.Class
import Control.Monad.Error.Class
import Control.Monad.Fail
import Control.Monad.Fix
import Control.Monad.Morph
import Control.Monad.Reader
import Control.Monad.State.Class
import Control.Monad.Writer.Class
import Control.Monad.Zip
import Control.Newtype
import Data.Semigroup
import qualified GHC.Generics as G

-- | A newtype wrapper around ReaderT for lifted monoid instances.
-- Memonic: @A@ for alternative AReaderT which can be merged into "a" single AReaderT
newtype AReaderT r m a = AReaderT { unAReaderT :: ReaderT r m a }
    deriving
    ( G.Generic
    , MonadTrans
    , Monad
    , Functor
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

type AReader r  = AReaderT r Identity

-- pattern AReaderT' :: (r -> m a) -> AReaderT r m a
-- pattern AReaderT' f = AReaderT (ReaderT f)

-- #if __GLASGOW_HASKELL__ >= 802
-- {-# COMPLETE AReaderT' #-}
-- #endif

areaderT :: (r -> m a) -> AReaderT r m a
areaderT = AReaderT . ReaderT

areader :: (r -> a) -> AReader r a
areader = AReaderT . reader

runAReaderT :: AReaderT r m a -> r -> m a
runAReaderT = runReaderT . unAReaderT

runAReader :: AReader r a -> r -> a
runAReader = runReader . unAReaderT

mapAReaderT :: (m a -> n b) -> AReaderT r m a -> AReaderT r n b
mapAReaderT f = AReaderT . mapReaderT f . unAReaderT

mapAReader :: (a -> b) -> AReader r a -> AReader r b
mapAReader f = AReaderT . mapReader f . unAReaderT

withAReaderT ::
    (r' -> r)        -- ^ The function to modify the environment.
    -> AReaderT r m a    -- ^ Computation to run in the modified environment.
    -> AReaderT r' m a
withAReaderT f = AReaderT . withReaderT f . unAReaderT

withAReader ::
    (r' -> r)        -- ^ The function to modify the environment.
    -> AReader r a       -- ^ Computation to run in the modified environment.
    -> AReader r' a
withAReader = withAReaderT

instance Newtype (AReaderT r m a)

type instance Magnified (AReaderT r m) = Magnified (ReaderT r m)
instance Monad m => Magnify (AReaderT s m) (AReaderT t m) s t where
    magnify l (AReaderT f) = AReaderT (magnify l f)

type instance Zoomed (AReaderT e m) = Zoomed (ReaderT e m)
instance Zoom m n s t => Zoom (AReaderT e m) (AReaderT e n) s t where
    zoom l (AReaderT f) = AReaderT (zoom l f)

-- | This is the reason for the newtye wrapper
-- This is different from the Alternative/MonadPlus instance.
-- The Alternative/MonadPlus instance runs one or the other
-- The Semigroup/Monoid instance runs both.
-- This Semigroup instance is the same as @(->) r@
instance (Semigroup a, Monad m) => Semigroup (AReaderT r m a) where
    (AReaderT f) <> (AReaderT g) = AReaderT (liftA2 (<>) f g)

-- | This is the reason for the newtye wrapper
-- This is different from the Alternative/MonadPlus instance.
-- The Alternative/MonadPlus instance runs one or the other
-- The Semigroup/Monoid instances runs both.
-- This Monoid instance is the same as @(->) r@
instance (Monoid a, Monad m) => Monoid (AReaderT r m a) where
    mempty = AReaderT (pure mempty)
    (AReaderT f) `mappend` (AReaderT g) = AReaderT (liftA2 mappend f g)
