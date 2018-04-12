{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Monad.Delegate.Class where

import Control.Monad.Trans.ACont
import Control.Monad.Trans.AReader
import qualified Control.Monad.Trans.ARWS.Lazy as Lazy
import qualified Control.Monad.Trans.ARWS.Strict as Strict
import Control.Monad.Trans.AState.Lazy as Lazy
import Control.Monad.Trans.AState.Strict as Strict
import Control.Monad.Trans.Class
import Control.Monad.Trans.Cont
import Control.Monad.Trans.Except
import Control.Monad.Trans.Identity
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
import qualified Control.Monad.Trans.RWS.Lazy as Lazy
import qualified Control.Monad.Trans.RWS.Strict as Strict
import Control.Monad.Trans.State.Lazy as Lazy
import Control.Monad.Trans.State.Strict as Strict
import Control.Monad.Trans.Writer.Lazy as Lazy
import Control.Monad.Trans.Writer.Strict as Strict
import Data.Functor.Identity

class Monad n => MonadDelegate r n | n -> r where
    delegate :: ((a -> r) -> r) -> n a

type MonadDelegate' r n = MonadDelegate (Identity r) n

delegate' :: MonadDelegate (Identity r) n => ((a -> r) -> r) -> n a
delegate' k = delegate (Identity . k . (runIdentity .))

instance (MonadDelegate r n) => MonadDelegate r (IdentityT n) where
    delegate = lift . delegate

instance MonadDelegate (m r) (ContT r m) where
    delegate = ContT

instance MonadDelegate (m r) (AContT r m) where
    delegate = acontT

instance (MonadDelegate r n) => MonadDelegate r (ExceptT e n) where
    delegate = lift . delegate

instance (MonadDelegate r n) => MonadDelegate r (MaybeT n) where
    delegate = lift . delegate

instance (MonadDelegate r n) => MonadDelegate r (ReaderT env n) where
    delegate = lift . delegate

instance (MonadDelegate r n) => MonadDelegate r (AReaderT env n) where
    delegate = lift . delegate

instance (MonadDelegate r n) => MonadDelegate r (Lazy.StateT s n) where
    delegate = lift . delegate

instance (MonadDelegate r n) => MonadDelegate r (Strict.StateT s n) where
    delegate = lift . delegate

instance (MonadDelegate r n) => MonadDelegate r (Lazy.AStateT s n) where
    delegate = lift . delegate

instance (MonadDelegate r n) => MonadDelegate r (Strict.AStateT s n) where
    delegate = lift . delegate

instance (MonadDelegate r n, Monoid w) => MonadDelegate r (Lazy.WriterT w n) where
    delegate = lift . delegate

instance (MonadDelegate r n, Monoid w) => MonadDelegate r (Strict.WriterT w n) where
    delegate = lift . delegate

instance (MonadDelegate r n, Monoid w) => MonadDelegate r (Lazy.RWST r w s n) where
    delegate = lift . delegate

instance (MonadDelegate r n, Monoid w) => MonadDelegate r (Strict.RWST r w s n) where
    delegate = lift . delegate

instance (MonadDelegate r n, Monoid w) => MonadDelegate r (Lazy.ARWST r w s n) where
    delegate = lift . delegate

instance (MonadDelegate r n, Monoid w) => MonadDelegate r (Strict.ARWST r w s n) where
    delegate = lift . delegate
