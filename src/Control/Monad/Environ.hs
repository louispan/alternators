{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Monad.Environ where

import Control.Monad.Reader
import Control.Monad.RWS.Lazy as Lazy
import Control.Monad.RWS.Strict as Strict
import Control.Monad.State.Lazy as Lazy
import Control.Monad.State.Strict as Strict

-- | like 'MonadReader' but with overlapping instances
-- Use newtype wrappers or 'Tagged' to ensure unique @r@ in the monad stack.
class Monad m => MonadAsk r m where
    askEnviron :: m r

-- | Any transformer on top of 'MonadAsk' is also a 'MonadAsk'
instance {-# OVERLAPPABLE #-} (Monad (t m), MonadTrans t, MonadAsk r m) => MonadAsk r (t m) where
    askEnviron = lift askEnviron

instance {-# OVERLAPPABLE #-} Monad m => MonadAsk r (ReaderT r m) where
    askEnviron = ask

instance {-# OVERLAPPABLE #-} Monad m => MonadAsk r (Strict.StateT r m) where
    askEnviron = get

instance {-# OVERLAPPABLE #-} Monad m => MonadAsk r (Lazy.StateT r m) where
    askEnviron = get

instance {-# OVERLAPPABLE #-} (Monoid w, Monad m) => MonadAsk r (Strict.RWST r w s m) where
    askEnviron = ask

instance {-# OVERLAPPABLE #-} (Monoid w, Monad m) => MonadAsk r (Lazy.RWST r w s m) where
    askEnviron = ask

instance {-# OVERLAPPABLE #-} (Monoid w, Monad m) => MonadAsk s (Strict.RWST r w s m) where
    askEnviron = get

instance {-# OVERLAPPABLE #-} (Monoid w, Monad m) => MonadAsk s (Lazy.RWST r w s m) where
    askEnviron = get

-- | like 'MonadState' but with overlapping instances
class MonadAsk s m => MonadPut s m where
    putEnviron :: s -> m ()
    -- getEnviron :: m s
    -- getEnviron = askEnviron (Proxy :: Proxy s)

instance {-# OVERLAPPABLE #-} (Monad (t m), MonadTrans t, MonadPut s m) => MonadPut s (t m) where
    putEnviron = lift . putEnviron

instance {-# OVERLAPPABLE #-} Monad m => MonadPut s (Strict.StateT s m) where
    putEnviron = put

instance {-# OVERLAPPABLE #-} Monad m => MonadPut s (Lazy.StateT s m) where
    putEnviron = put

instance {-# OVERLAPPABLE #-} (Monoid w, Monad m) => MonadPut s (Strict.RWST r w s m) where
    putEnviron = put

instance {-# OVERLAPPABLE #-} (Monoid w, Monad m) => MonadPut s (Lazy.RWST r w s m) where
    putEnviron = put

-- | Using ScopedTypeVariables to specify the type of @s@ into 'askEnviron'
modifyEnviron :: forall s m. MonadPut s m => (s -> s) -> m ()
modifyEnviron f = do
    s <- askEnviron @s
    putEnviron $! f s
