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

class Monad m => MonadDelegate r m | m -> r where
    delegate :: ((a -> r) -> r) -> m a

type MonadDelegate' r m = MonadDelegate (Identity r) m

delegate' :: MonadDelegate (Identity r) m => ((a -> r) -> r) -> m a
delegate' k = delegate (Identity . k . (runIdentity .))

instance MonadDelegate (m r) (ContT r m) where
    delegate = ContT

instance MonadDelegate (m r) (AContT r m) where
    delegate = acontT

instance (MonadDelegate r m) => MonadDelegate r (IdentityT m) where
    delegate = lift . delegate

instance (MonadDelegate r m) => MonadDelegate r (ExceptT e m) where
    delegate = lift . delegate

instance (MonadDelegate r m) => MonadDelegate r (MaybeT m) where
    delegate = lift . delegate

instance (MonadDelegate r m) => MonadDelegate r (ReaderT env m) where
    delegate = lift . delegate

instance (MonadDelegate r m) => MonadDelegate r (AReaderT env m) where
    delegate = lift . delegate

instance (MonadDelegate r m) => MonadDelegate r (Lazy.StateT s m) where
    delegate = lift . delegate

instance (MonadDelegate r m) => MonadDelegate r (Strict.StateT s m) where
    delegate = lift . delegate

instance (MonadDelegate r m) => MonadDelegate r (Lazy.AStateT s m) where
    delegate = lift . delegate

instance (MonadDelegate r m) => MonadDelegate r (Strict.AStateT s m) where
    delegate = lift . delegate

instance (MonadDelegate r m, Monoid w) => MonadDelegate r (Lazy.WriterT w m) where
    delegate = lift . delegate

instance (MonadDelegate r m, Monoid w) => MonadDelegate r (Strict.WriterT w m) where
    delegate = lift . delegate

instance (MonadDelegate r m, Monoid w) => MonadDelegate r (Lazy.RWST r w s m) where
    delegate = lift . delegate

instance (MonadDelegate r m, Monoid w) => MonadDelegate r (Strict.RWST r w s m) where
    delegate = lift . delegate

instance (MonadDelegate r m, Monoid w) => MonadDelegate r (Lazy.ARWST r w s m) where
    delegate = lift . delegate

instance (MonadDelegate r m, Monoid w) => MonadDelegate r (Strict.ARWST r w s m) where
    delegate = lift . delegate
