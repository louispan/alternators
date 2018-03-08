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

module Control.Monad.Trans.Delegate where

import Control.Applicative
import Control.Monad.Cont
import Control.Monad.Fail
import Control.Monad.Reader
import Control.Monad.State.Class
import Control.Monad.Zip
import Control.Newtype
import Data.Coerce
import Data.Semigroup
import qualified GHC.Generics as G

-- | A 'Delegate' is a @ContT () m a@ which allows for Monoid and Semigroup instances
newtype DelegateT m a = DelegateT { runDelegateT :: ContT () m a }
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

pattern DelegateT' :: ((a -> m ()) -> m ()) -> DelegateT m a
pattern DelegateT' f = DelegateT (ContT f)

#if __GLASGOW_HASKELL__ >= 802
{-# COMPLETE ReadrT_ #-}
#endif

delegateT' :: ((a -> m ()) -> m ()) -> DelegateT m a
delegateT' = coerce

runDelegateT' :: DelegateT m a -> (a -> m ()) -> m ()
runDelegateT' = coerce

instance Newtype (DelegateT m a)

deriving instance MonadState s (ContT () m) => MonadState s (DelegateT m)

-- instance MonadTrans (DelegateT r) where
--     lift = Delegate . lift . lift

instance MonadZip (DelegateT m) where
    mzip x y = liftA2 (,) x y

-- | This is the reason for the newtye wrapper
-- This is different from the Alternative/MonadPlus instance.
-- The Alternative/MonadPlus instance runs one or the other
-- The Semigroup/Monoid instances runs both, and fires the output twice.
instance Applicative m => Semigroup (DelegateT m c) where
    DelegateT (ContT f) <> DelegateT (ContT g) =
        DelegateT . ContT $ \k -> f k *> g k

-- | This is the reason for the newtye wrapper
-- This is different from the Alternative/MonadPlus instance.
-- The Alternative/MonadPlus instance runs one or the other
-- The Semigroup/Monoid instances runs both, and fires the output twice.
instance Applicative m => Monoid (DelegateT m c) where
    mempty = DelegateT . ContT . const $ pure ()
    mappend = (<>)

-- | ContT didn't have an instance of Alternative1
instance Alternative m => Alternative (DelegateT m) where
    empty = DelegateT $ ContT $ const empty
    DelegateT (ContT f) <|> DelegateT (ContT g) =
        DelegateT $ ContT $ \k -> f k <|> g k

instance MonadPlus m => MonadPlus (DelegateT m) where
    mzero = empty
    mplus = (<|>)
