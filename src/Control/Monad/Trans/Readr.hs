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

#if __GLASGOW_HASKELL__ < 802
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
#endif

module Control.Monad.Trans.Readr where

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
import Data.Coerce
import Data.Semigroup
import qualified GHC.Generics as G

-- | A newtype wrapper around ReaderT for custom monoid instances
newtype ReadrT r m a = ReadrT { runReadrT :: ReaderT r m a }
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
    , MFunctor
    , MMonad
    )

pattern ReadrT' :: (r -> m a) -> ReadrT r m a
pattern ReadrT' f = ReadrT (ReaderT f)

#if __GLASGOW_HASKELL__ >= 802
{-# COMPLETE ReadrT_ #-}
#endif

readrT' :: (r -> m a) -> ReadrT r m a
readrT' = coerce

runReadrT' :: ReadrT r m a -> r -> m a
runReadrT' = coerce

instance Newtype (ReadrT r m a)

deriving instance MonadWriter w m => MonadWriter w (ReadrT r m)
deriving instance MonadState s m => MonadState s (ReadrT r m)
deriving instance MonadError e m => MonadError e (ReadrT r m)
deriving instance MonadCont m => MonadCont (ReadrT r m)

type instance Magnified (ReadrT r m) = Magnified (ReaderT r m)
instance Monad m => Magnify (ReadrT s m) (ReadrT t m) s t where
    magnify l (ReadrT f) = ReadrT (magnify l f)

type instance Zoomed (ReadrT e m) = Zoomed (ReaderT e m)
instance Zoom m n s t => Zoom (ReadrT e m) (ReadrT e n) s t where
    zoom l (ReadrT f) = ReadrT (zoom l f)

-- | This is the reason for the newtye wrapper
-- This is different from the Alternative/MonadPlus instance.
-- The Alternative/MonadPlus instance runs one or the other
-- The Semigroup/Monoid instance runs both.
-- This Semigroup instance is the same as @(->) r@
instance (Semigroup (m a)) => Semigroup (ReadrT r m a) where
    (ReadrT (ReaderT f)) <> (ReadrT (ReaderT g)) = ReadrT (ReaderT (f <> g))

-- | This is the reason for the newtye wrapper
-- This is different from the Alternative/MonadPlus instance.
-- The Alternative/MonadPlus instance runs one or the other
-- The Semigroup/Monoid instances runs both.
-- This Monoid instance is the same as @(->) r@
instance (Monoid (m a)) => Monoid (ReadrT r m a) where
    mempty = ReadrT (ReaderT mempty)
    (ReadrT (ReaderT f)) `mappend` (ReadrT (ReaderT g)) = ReadrT (ReaderT (f `mappend` g))
