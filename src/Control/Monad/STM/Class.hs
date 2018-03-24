{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE UndecidableInstances #-}

module Control.Monad.STM.Class where

import Control.Monad.STM
import Control.Monad.Trans.Class
import Control.Monad.Trans.Cont
import Control.Monad.Trans.Conts
import Control.Monad.Trans.Except
import Control.Monad.Trans.Identity
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.MCont
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Readers
import qualified Control.Monad.Trans.RWS.Lazy as Lazy
import qualified Control.Monad.Trans.RWS.Strict as Strict
import qualified Control.Monad.Trans.RWSs.Lazy as Lazy
import qualified Control.Monad.Trans.RWSs.Strict as Strict
import Control.Monad.Trans.State.Lazy as Lazy
import Control.Monad.Trans.State.Strict as Strict
import Control.Monad.Trans.States.Lazy as Lazy
import Control.Monad.Trans.States.Strict as Strict
import Control.Monad.Trans.Writer.Lazy as Lazy
import Control.Monad.Trans.Writer.Strict as Strict

-- | From https://gist.github.com/snoyberg/199fd17643dded22d9cc8812b0ae6ed9
-- except no instance for IO
class Monad m => MonadSTM m where
    liftSTM :: STM a -> m a

instance MonadSTM STM where
    liftSTM = id

instance MonadSTM m => MonadSTM (IdentityT m) where
    liftSTM = lift . liftSTM

instance MonadSTM m => MonadSTM (ContT r m) where
    liftSTM = lift . liftSTM

instance MonadSTM m => MonadSTM (ContsT r m) where
    liftSTM = lift . liftSTM

instance MonadSTM m => MonadSTM (MContT r m) where
    liftSTM = lift . liftSTM

instance MonadSTM m => MonadSTM (ExceptT e m) where
    liftSTM = lift . liftSTM

instance MonadSTM m => MonadSTM (MaybeT m) where
    liftSTM = lift . liftSTM

instance MonadSTM m => MonadSTM (ReaderT r m) where
    liftSTM = lift . liftSTM

instance MonadSTM m => MonadSTM (ReadersT r m) where
    liftSTM = lift . liftSTM

instance MonadSTM m => MonadSTM (Lazy.StateT s m) where
    liftSTM = lift . liftSTM

instance MonadSTM m => MonadSTM (Strict.StateT s m) where
    liftSTM = lift . liftSTM

instance MonadSTM m => MonadSTM (Lazy.StatesT s m) where
    liftSTM = lift . liftSTM

instance MonadSTM m => MonadSTM (Strict.StatesT s m) where
    liftSTM = lift . liftSTM

instance (Monoid w, MonadSTM m) => MonadSTM (Lazy.WriterT w m) where
    liftSTM = lift . liftSTM

instance (Monoid w, MonadSTM m) => MonadSTM (Strict.WriterT w m) where
    liftSTM = lift . liftSTM

instance (Monoid w, MonadSTM m) => MonadSTM (Lazy.RWST r w s m) where
    liftSTM = lift . liftSTM

instance (Monoid w, MonadSTM m) => MonadSTM (Strict.RWST r w s m) where
    liftSTM = lift . liftSTM

instance (Monoid w, MonadSTM m) => MonadSTM (Lazy.RWSsT r w s m) where
    liftSTM = lift . liftSTM

instance (Monoid w, MonadSTM m) => MonadSTM (Strict.RWSsT r w s m) where
    liftSTM = lift . liftSTM
