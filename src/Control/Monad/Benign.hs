{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Control.Monad.Benign where

import Control.Also
import Control.Applicative
import Control.Monad.Morph
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
import qualified GHC.Generics as G

#if MIN_VERSION_base(4,9,0) && !MIN_VERSION_base(4,10,0)
import Data.Semigroup
#endif

-- | A wrapper to indicate that the enclosed monad is non-blocking and benign.
-- Running 'Benign' effect multiple times should have no noticeable extra effects.
-- The only side effect of a Benign is it may keep a reference to an IORef
-- preventing garbage collection if you keep the Benign in scope.
newtype Benign m a = Benign (m a)
    deriving (G.Generic, G.Generic1, Functor, Applicative, Monad)

instance Show (Benign m a) where
    showsPrec _ _ = showString "Benign"

instance MonadTrans Benign where
    lift = Benign

instance MFunctor Benign where
    hoist f (Benign m) = Benign (f m)

instance MMonad Benign where
    embed f (Benign m) = f m

instance (Semigroup a, Applicative m) => Semigroup (Benign m a) where
    Benign f <> Benign g = Benign $ liftA2 (<>) f g

instance (Monoid a, Applicative m) => Monoid (Benign m a) where
    mempty = Benign $ pure mempty
#if !MIN_VERSION_base(4,11,0)
    (Benign f) `mappend` (Benign g) = Benign (liftA2 mappend f g)
#endif

instance (Also a m) => Also a (Benign m) where
    alsoZero = Benign alsoZero
    (Benign a) `also` (Benign b) = Benign $ a `also` b

getBenign :: Benign m a -> m a
getBenign (Benign m) = m

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


