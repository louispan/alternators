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

module Control.Monad.Trans.Readers where

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
-- Memonic: the @s@ means plural, alluding to the monoidal property.
newtype ReadersT r m a = ReadersT { unReadersT :: ReaderT r m a }
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

type Readers r  = ReadersT r Identity

-- pattern ReadersT' :: (r -> m a) -> ReadersT r m a
-- pattern ReadersT' f = ReadersT (ReaderT f)

-- #if __GLASGOW_HASKELL__ >= 802
-- {-# COMPLETE ReadersT' #-}
-- #endif

readersT :: (r -> m a) -> ReadersT r m a
readersT = ReadersT . ReaderT

readers :: (r -> a) -> Readers r a
readers = ReadersT . reader

runReadersT :: ReadersT r m a -> r -> m a
runReadersT = runReaderT . unReadersT

runReaders :: Readers r a -> r -> a
runReaders = runReader . unReadersT

mapReadersT :: (m a -> n b) -> ReadersT r m a -> ReadersT r n b
mapReadersT f = ReadersT . mapReaderT f . unReadersT

mapReaders :: (a -> b) -> Readers r a -> Readers r b
mapReaders f = ReadersT . mapReader f . unReadersT

withReadersT ::
    (r' -> r)        -- ^ The function to modify the environment.
    -> ReadersT r m a    -- ^ Computation to run in the modified environment.
    -> ReadersT r' m a
withReadersT f = ReadersT . withReaderT f . unReadersT

withReader ::
    (r' -> r)        -- ^ The function to modify the environment.
    -> Reader r a       -- ^ Computation to run in the modified environment.
    -> Reader r' a
withReader = withReaderT

instance Newtype (ReadersT r m a)

type instance Magnified (ReadersT r m) = Magnified (ReaderT r m)
instance Monad m => Magnify (ReadersT s m) (ReadersT t m) s t where
    magnify l (ReadersT f) = ReadersT (magnify l f)

type instance Zoomed (ReadersT e m) = Zoomed (ReaderT e m)
instance Zoom m n s t => Zoom (ReadersT e m) (ReadersT e n) s t where
    zoom l (ReadersT f) = ReadersT (zoom l f)

-- | This is the reason for the newtye wrapper
-- This is different from the Alternative/MonadPlus instance.
-- The Alternative/MonadPlus instance runs one or the other
-- The Semigroup/Monoid instance runs both.
-- This Semigroup instance is the same as @(->) r@
instance (Semigroup a, Monad m) => Semigroup (ReadersT r m a) where
    (ReadersT f) <> (ReadersT g) = ReadersT (liftA2 (<>) f g)

-- | This is the reason for the newtye wrapper
-- This is different from the Alternative/MonadPlus instance.
-- The Alternative/MonadPlus instance runs one or the other
-- The Semigroup/Monoid instances runs both.
-- This Monoid instance is the same as @(->) r@
instance (Monoid a, Monad m) => Monoid (ReadersT r m a) where
    mempty = ReadersT (pure mempty)
    (ReadersT f) `mappend` (ReadersT g) = ReadersT (liftA2 mappend f g)
