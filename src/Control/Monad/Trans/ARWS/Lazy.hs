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

module Control.Monad.Trans.ARWS.Lazy where

import Control.Applicative
import Control.Lens
import Control.Monad.Cont.Class
import Control.Monad.Error.Class
import Control.Monad.Fail
import Control.Monad.Fix
import Control.Monad.Morph
import Control.Monad.Reader
import Control.Monad.RWS.Lazy hiding ((<>))
import Control.Newtype
import Data.Semigroup
import qualified GHC.Generics as G

-- | A newtype wrapper around RWST for lifted monoid instances.
-- Memonic: @A@ for alternative RWST which can be merged into "a" single RWST
newtype ARWST r w s m a = ARWST { unARWST :: RWST r w s m a }
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

type ARWS r w s  = ARWST r w s Identity

-- pattern ARWST' :: (r -> s -> m (a, s, w)) -> ARWST r w s m a
-- pattern ARWST' f = ARWST (RWST f)

-- #if __GLASGOW_HASKELL__ >= 802
-- {-# COMPLETE ARWST' #-}
-- #endif

arwsT :: (r -> s -> m (a, s, w)) -> ARWST r w s m a
arwsT = ARWST . RWST

arws :: (r -> s -> (a, s, w)) -> ARWS r w s a
arws = ARWST . rws

runARWST :: ARWST r w s m a -> r -> s -> m (a, s, w)
runARWST = runRWST . unARWST

runARWS :: ARWS r w s a -> r -> s -> (a, s, w)
runARWS = runRWS . unARWST

evalARWST :: Monad m => ARWST r w s m a -> r -> s -> m (a, w)
evalARWST = evalRWST . unARWST

evalARWS :: ARWS r w s a -> r -> s -> (a, w)
evalARWS = evalRWS . unARWST

execARWST :: Monad m => ARWST r w s m a -> r -> s -> m (s, w)
execARWST = execRWST . unARWST

execARWS :: ARWS r w s a -> r -> s -> (s, w)
execARWS = execRWS . unARWST

mapARWST :: (m (a, s, w) -> n (b, s, w')) -> ARWST r w s m a -> ARWST r w' s n b
mapARWST f = ARWST . mapRWST f . unARWST

mapARWS :: ((a, s, w) -> (b, s, w')) -> ARWS r w s a -> ARWS r w' s b
mapARWS f = ARWST . mapRWS f . unARWST

withARWST :: (r' -> s -> (r, s)) -> ARWST r w s m a -> ARWST r' w s m a
withARWST f = ARWST . withRWST f . unARWST

withARWS :: (r' -> s -> (r, s)) -> ARWS r w s a -> ARWS r' w s a
withARWS = withARWST

instance Newtype (ARWST r w s m a)

type instance Magnified (ARWST r w s m) = Magnified (RWST r w s m)
instance (Monad m, Monoid w) => Magnify (ARWST a w s m) (ARWST b w s m) a b where
    magnify l (ARWST f) = ARWST (magnify l f)

type instance Zoomed (ARWST r w s m) = Zoomed (RWST r w s m)
instance (Monad m, Monoid w) => Zoom (ARWST r w s m) (ARWST r w t m) s t where
    zoom l (ARWST f) = ARWST (zoom l f)

-- | This is the reason for the newtye wrapper
instance (Monad m, Semigroup (m a), Semigroup w) => Semigroup (ARWST r w s m a) where
    (ARWST (RWST f)) <> (ARWST (RWST g)) = ARWST . RWST $ \r s -> do
        (a1, s1, w1) <- f r s
        (a2, s2, w2) <- g r s1
        (\a3 -> (a3, s2, w1 <> w2)) <$> ((pure a1) <> (pure a2))

-- | This is the reason for the newtye wrapper
instance (Monad m, Monoid (m a), Monoid w) => Monoid (ARWST r w s m a) where
    mempty = ARWST . RWST $ \_ s -> (\a -> (a, s, mempty)) <$> mempty
    (ARWST (RWST f)) `mappend` (ARWST (RWST g)) = ARWST . RWST $ \r s -> do
        (a1, s1, w1) <- f r s
        (a2, s2, w2) <- g r s1
        (\a3 -> (a3, s2, w1 `mappend` w2)) <$> ((pure a1) `mappend` (pure a2))
