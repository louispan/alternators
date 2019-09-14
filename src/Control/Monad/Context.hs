{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Monad.Context where

import Control.Monad.Reader
import Control.Monad.RWS.Lazy as Lazy
import Control.Monad.RWS.Strict as Strict
import Control.Monad.State.Lazy as Lazy
import Control.Monad.State.Strict as Strict

-- | like 'MonadReader' but with overlapping instances
-- Use newtype wrappers or 'Tagged' to ensure unique @r@ in the monad stack.
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

-- | like 'MonadState' but with overlapping instances
class MonadAsk s m => MonadPut s m where
    putContext :: s -> m ()
    -- getContext :: m s
    -- getContext = askContext (Proxy :: Proxy s)

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

-- | Using ScopedTypeVariables to specify the type of @s@ into 'askContext'
modifyContext :: forall s m. MonadPut s m => (s -> s) -> m ()
modifyContext f = do
    s <- askContext @s
    putContext $! f s
