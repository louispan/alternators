{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

#if __GLASGOW_HASKELL__ < 802
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
#endif

module Control.Monad.Trans.RWSs.Strict where

import Control.Applicative
import Control.Lens
import Control.Monad.Cont.Class
import Control.Monad.Error.Class
import Control.Monad.Fail
import Control.Monad.Fix
import Control.Monad.Morph
import Control.Monad.Reader
import Control.Monad.RWS.Strict hiding ((<>))
import Control.Newtype
import Data.Coerce
import Data.Semigroup
import qualified GHC.Generics as G

-- | A newtype wrapper around RWST for lifted monoid instances.
-- Memonic: the @s@ means plural, alluding to the monoidal property.
newtype RWSsT r w s m a = RWSsT { runRWSsT :: RWST r w s m a }
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

type RWSs r w s  = RWSsT r w s Identity

pattern RWSsT' :: (r -> s -> m (a, s, w)) -> RWSsT r w s m a
pattern RWSsT' f = RWSsT (RWST f)

#if __GLASGOW_HASKELL__ >= 802
{-# COMPLETE RWSsT' #-}
#endif

rwssT' :: (r -> s -> m (a, s, w)) -> RWSsT r w s m a
rwssT' = coerce

rwss' :: (r -> s -> (a, s, w)) -> RWSs r w s a
rwss' k = RWSsT (rws k)

runRWSsT' :: RWSsT r w s m a -> r -> s -> m (a, s, w)
runRWSsT' = coerce

runRWSs' :: RWSs r w s a -> r -> s -> (a, s, w)
runRWSs' (RWSsT m) = runRWS m

instance Newtype (RWSsT r w s m a)

type instance Magnified (RWSsT r w s m) = Magnified (RWST r w s m)
instance (Monad m, Monoid w) => Magnify (RWSsT a w s m) (RWSsT b w s m) a b where
    magnify l (RWSsT f) = RWSsT (magnify l f)

type instance Zoomed (RWSsT r w s m) = Zoomed (RWST r w s m)
instance (Monad m, Monoid w) => Zoom (RWSsT r w s m) (RWSsT r w t m) s t where
    zoom l (RWSsT f) = RWSsT (zoom l f)

-- | This is the reason for the newtye wrapper
-- This is different from the Alternative/MonadPlus instance.
-- The Alternative/MonadPlus instance runs one or the other
-- The Semigroup/Monoid instance runs both.
-- This Semigroup instance is the same as @(->) r@
instance (Monad m, Semigroup a, Monoid w) => Semigroup (RWSsT r w s m a) where
    (RWSsT f) <> (RWSsT g) = RWSsT (liftA2 (<>) f g)

-- | This is the reason for the newtye wrapper
-- This is different from the Alternative/MonadPlus instance.
-- The Alternative/MonadPlus instance runs one or the other
-- The Semigroup/Monoid instances runs both.
-- This Monoid instance is the same as @(->) r@
instance (Monad m, Monoid a, Monoid w) => Monoid (RWSsT r w s m a) where
    mempty = RWSsT (pure mempty)
    (RWSsT f) `mappend` (RWSsT g) = RWSsT (liftA2 mappend f g)
