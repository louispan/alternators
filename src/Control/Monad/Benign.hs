{-# LANGUAGE FlexibleInstances #-}

module Control.Monad.Benign
 ( Benign -- don't export constructor
 , getBenign
 , benignReadIORef
 , benignDeRefWeak
 , MonadBenignIO(..)
 ) where

import Control.Monad.Benign.Internal
import Control.Monad.Trans
import Control.Monad.Trans.Cont
import Control.Monad.Trans.Except
import Control.Monad.Trans.Identity
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
import Control.Monad.Trans.RWS.Lazy as Lazy
import Control.Monad.Trans.RWS.Strict as Strict
import Control.Monad.Trans.State.Lazy as Lazy
import Control.Monad.Trans.State.Strict as Strict
import Control.Monad.Trans.Writer.Lazy as Lazy
import Control.Monad.Trans.Writer.Strict as Strict
import Data.IORef
import System.Mem.Weak

class Monad m => MonadBenignIO m where
    -- | Lift a computation from the 'Benign IO' monad.
    liftBenignIO :: Benign IO a -> m a

instance MonadBenignIO IO where
    liftBenignIO = getBenign

instance MonadBenignIO (Benign IO) where
    liftBenignIO = id

instance (MonadBenignIO m) => MonadBenignIO (IdentityT m) where
    liftBenignIO = IdentityT . liftBenignIO

instance (MonadBenignIO m) => MonadBenignIO (MaybeT m) where
    liftBenignIO = lift . liftBenignIO

instance (MonadBenignIO m) => MonadBenignIO (ReaderT r m) where
    liftBenignIO = lift . liftBenignIO

instance (MonadBenignIO m) => MonadBenignIO (ContT r m) where
    liftBenignIO = lift . liftBenignIO

instance (MonadBenignIO m) => MonadBenignIO (ExceptT e m) where
    liftBenignIO = lift . liftBenignIO

instance (MonadBenignIO m) => MonadBenignIO (Lazy.StateT s m) where
    liftBenignIO = lift . liftBenignIO

instance (MonadBenignIO m) => MonadBenignIO (Strict.StateT s m) where
    liftBenignIO = lift . liftBenignIO

instance (Monoid w, MonadBenignIO m) => MonadBenignIO (Lazy.WriterT w m) where
    liftBenignIO = lift . liftBenignIO

instance (Monoid w, MonadBenignIO m) => MonadBenignIO (Strict.WriterT w m) where
    liftBenignIO = lift . liftBenignIO

instance (Monoid w, MonadBenignIO m) => MonadBenignIO (Lazy.RWST r w s m) where
    liftBenignIO = lift . liftBenignIO

instance (Monoid w, MonadBenignIO m) => MonadBenignIO (Strict.RWST r w s m) where
    liftBenignIO = lift . liftBenignIO

benignReadIORef :: MonadBenignIO m => IORef a -> m a
benignReadIORef ref = liftBenignIO $ Benign $ readIORef $ ref

benignDeRefWeak :: MonadBenignIO m => Weak a -> m (Maybe a)
benignDeRefWeak ref = liftBenignIO $ Benign $ deRefWeak $ ref
