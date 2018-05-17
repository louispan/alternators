{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Monad.Devolve.Class where

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

class Monad m => MonadDevolve r m | m -> r where
    devolve :: ((a -> r) -> r) -> m a

type MonadDevolve' r m = MonadDevolve (Identity r) m

devolve' :: MonadDevolve (Identity r) m => ((a -> r) -> r) -> m a
devolve' k = devolve (Identity . k . (runIdentity .))

instance MonadDevolve (m r) (ContT r m) where
    devolve = ContT

instance MonadDevolve (m r) (AContT r m) where
    devolve = acontT

instance (MonadDevolve r m) => MonadDevolve r (IdentityT m) where
    devolve = lift . devolve

instance (MonadDevolve r m) => MonadDevolve r (ExceptT e m) where
    devolve = lift . devolve

instance (MonadDevolve r m) => MonadDevolve r (MaybeT m) where
    devolve = lift . devolve

instance (MonadDevolve r m) => MonadDevolve r (ReaderT env m) where
    devolve = lift . devolve

instance (MonadDevolve r m) => MonadDevolve r (AReaderT env m) where
    devolve = lift . devolve

instance (MonadDevolve r m) => MonadDevolve r (Lazy.StateT s m) where
    devolve = lift . devolve

instance (MonadDevolve r m) => MonadDevolve r (Strict.StateT s m) where
    devolve = lift . devolve

instance (MonadDevolve r m) => MonadDevolve r (Lazy.AStateT s m) where
    devolve = lift . devolve

instance (MonadDevolve r m) => MonadDevolve r (Strict.AStateT s m) where
    devolve = lift . devolve

instance (MonadDevolve r m, Monoid w) => MonadDevolve r (Lazy.WriterT w m) where
    devolve = lift . devolve

instance (MonadDevolve r m, Monoid w) => MonadDevolve r (Strict.WriterT w m) where
    devolve = lift . devolve

instance (MonadDevolve r m, Monoid w) => MonadDevolve r (Lazy.RWST r w s m) where
    devolve = lift . devolve

instance (MonadDevolve r m, Monoid w) => MonadDevolve r (Strict.RWST r w s m) where
    devolve = lift . devolve

instance (MonadDevolve r m, Monoid w) => MonadDevolve r (Lazy.ARWST r w s m) where
    devolve = lift . devolve

instance (MonadDevolve r m, Monoid w) => MonadDevolve r (Strict.ARWST r w s m) where
    devolve = lift . devolve
