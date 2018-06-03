{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- #if __GLASGOW_HASKELL__ < 802
-- {-# OPTIONS_GHC -Wno-incomplete-patterns #-}
-- #endif

module Control.Monad.Trans.AExcept where

import Control.Applicative
import Control.Lens
import Control.Monad.Cont.Class
import Control.Monad.Error.Class
import Control.Monad.Except
import Control.Monad.Fail
import Control.Monad.Morph
import Control.Monad.Reader
import Control.Monad.RWS.Class
import Control.Monad.Trans.Maybe
import Control.Monad.Writer.Class
import Control.Monad.Zip
import Control.Newtype
import Data.Functor.Classes
import Data.Semigroup
import qualified GHC.Generics as G

-- | A newtype wrapper around ExceptT for lifted monoid instances.
-- Memonic: @A@ for alternative ExceptT which can be merged into "a" single MaybeT
newtype AExceptT e m a = AExceptT { unAExceptT :: ExceptT e m a }
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
    , MonadRWS r w s
    , MonadCont
    , MFunctor
    , MMonad
    )

-- pattern AMaybeT' :: (r -> m a) -> AMaybeT r m a
-- pattern AMaybeT' f = AMaybeT (ReaderT f)

-- #if __GLASGOW_HASKELL__ >= 802
-- {-# COMPLETE AMaybeT' #-}
-- #endif

aexceptT :: m (Either e a) -> AExceptT e m a
aexceptT = AExceptT . ExceptT

runAExceptT :: AExceptT e m a -> m (Either e a)
runAExceptT = runExceptT . unAExceptT

mapAExceptT :: (m (Either e a) -> n (Either e' b)) -> AExceptT e m a -> AExceptT e' n b
mapAExceptT f = AExceptT . mapExceptT f . unAExceptT

withAExceptT :: Functor m => (e -> e') -> AExceptT e m a -> AExceptT e' m a
withAExceptT f (AExceptT m) = AExceptT $ withExceptT f m

instance Newtype (AExceptT e m a)

type instance Zoomed (AExceptT e m) = Zoomed (ExceptT e m)
instance Zoom m n s t => Zoom (AExceptT e m) (AExceptT e n) s t where
    zoom l (AExceptT f) = AExceptT (zoom l f)

-- | This is the reason for the newtye wrapper
-- Unlike the Monad instance, the Semigroup instance always run both args.
-- Unlike other AXXX transformers, this instance relies on the inner monad to be
-- @Semigroup (m (Either e a))@, not just @Semigroup (m a))@.
instance (Semigroup (m (Either e a))) => Semigroup (AExceptT e m a) where
    (AExceptT (ExceptT f)) <> (AExceptT (ExceptT g)) = AExceptT . ExceptT $ f <> g

-- | This is the reason for the newtye wrapper
-- Unlike the Monad instance, the Monoid instance always run both args.
-- Unlike other AXXX transformers, this instance relies on the inner monad to be
-- @Monoid (m (Either e a))@, not just @Monoid (m a))@.
instance (Monoid (m (Either e a))) => Monoid (AExceptT e m a) where
    mempty = AExceptT . ExceptT $ mempty
    (AExceptT (ExceptT f)) `mappend` (AExceptT (ExceptT g)) = AExceptT . ExceptT $ f `mappend` g
