{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Monad.Context where

import Control.Monad.Reader
import Control.Monad.State.Strict as Strict
import Control.Monad.RWS.Strict as Strict
import Control.Monad.State.Lazy as Lazy
import Control.Monad.RWS.Lazy as Lazy

-- | A copy of 'MonadReader' with overlapping instances
class Monad m => MonadAsk r m where
    askContext :: m r

instance {-# OVERLAPPABLE #-} (Monad (t m), MonadTrans t, MonadAsk r m) => MonadAsk r (t m) where
    askContext = lift askContext

instance {-# OVERLAPPABLE #-} Monad m => MonadAsk r (ReaderT r m) where
    askContext = ask

instance {-# OVERLAPPABLE #-} Monad m => MonadAsk r (Strict.StateT r m) where
    askContext = get

instance {-# OVERLAPPABLE #-} Monad m => MonadAsk r (Lazy.StateT r m) where
    askContext = get

instance {-# OVERLAPPABLE #-} (Monoid w, Monad m) => MonadAsk r (Strict.RWST r w s m) where
    askContext = ask

instance {-# OVERLAPPABLE #-} (Monoid w, Monad m) => MonadAsk r (Lazy.RWST r w s m) where
    askContext = ask

instance {-# OVERLAPPABLE #-} (Monoid w, Monad m) => MonadAsk s (Strict.RWST r w s m) where
    askContext = get

instance {-# OVERLAPPABLE #-} (Monoid w, Monad m) => MonadAsk s (Lazy.RWST r w s m) where
    askContext = get

class MonadAsk s m => MonadPut s m where
    putContext :: s -> m ()

instance {-# OVERLAPPABLE #-} (Monad (t m), MonadTrans t, MonadPut s m) => MonadPut s (t m) where
    putContext = lift . putContext

instance {-# OVERLAPPABLE #-} Monad m => MonadPut s (Strict.StateT s m) where
    putContext = put

instance {-# OVERLAPPABLE #-} Monad m => MonadPut s (Lazy.StateT s m) where
    putContext = put

instance {-# OVERLAPPABLE #-} (Monoid w, Monad m) => MonadPut s (Strict.RWST r w s m) where
    putContext = put

instance {-# OVERLAPPABLE #-} (Monoid w, Monad m) => MonadPut s (Lazy.RWST r w s m) where
    putContext = put

modifyContext :: MonadPut s m => (s -> s) -> m ()
modifyContext f = do
  s <- askContext
  putContext $! f s

-- | A copy of 'MonadReader' with overlapping instances
class Monad m => MonadAsk' f r m | m -> r where
    askContext' :: m (f r)

instance {-# OVERLAPPABLE #-} (Monad (t m), MonadTrans t, MonadAsk' f r m) => MonadAsk' f r (t m) where
    askContext' = lift askContext'

instance {-# OVERLAPPABLE #-} Monad m => MonadAsk' f r (ReaderT (f r) m) where
    askContext' = ask

instance {-# OVERLAPPABLE #-} Monad m => MonadAsk' f r (Strict.StateT (f r) m) where
    askContext' = get

instance {-# OVERLAPPABLE #-} Monad m => MonadAsk' f r (Lazy.StateT (f r) m) where
    askContext' = get

instance {-# OVERLAPPABLE #-} (Monoid w, Monad m) => MonadAsk' f r (Strict.RWST (f r) w s m) where
    askContext' = ask

instance {-# OVERLAPPABLE #-} (Monoid w, Monad m) => MonadAsk' f r (Lazy.RWST (f r) w s m) where
    askContext' = ask

instance {-# OVERLAPPABLE #-} (Monoid w, Monad m) => MonadAsk' f s (Strict.RWST r w (f s) m) where
    askContext' = get

instance {-# OVERLAPPABLE #-} (Monoid w, Monad m) => MonadAsk' f s (Lazy.RWST r w (f s) m) where
    askContext' = get

class MonadAsk' f s m => MonadPut' f s m | m -> s where
    putContext' :: f s -> m ()

instance {-# OVERLAPPABLE #-} (Monad (t m), MonadTrans t, MonadPut' f s m) => MonadPut' f s (t m) where
    putContext' = lift . putContext'

instance {-# OVERLAPPABLE #-} Monad m => MonadPut' f s (Strict.StateT (f s) m) where
    putContext' = put

instance {-# OVERLAPPABLE #-} Monad m => MonadPut' f s (Lazy.StateT (f s) m) where
    putContext' = put

instance {-# OVERLAPPABLE #-} (Monoid w, Monad m) => MonadPut' f s (Strict.RWST r w (f s) m) where
    putContext' = put

instance {-# OVERLAPPABLE #-} (Monoid w, Monad m) => MonadPut' f s (Lazy.RWST r w (f s) m) where
    putContext' = put

modifyContext' :: MonadPut' f s m => (f s -> f s) -> m ()
modifyContext' f = do
  s <- askContext'
  putContext' $! f s
