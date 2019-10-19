{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Monad.Environ
    ( module Data.Proxy
    , MonadAsk(..)
    , MonadAsk'
    , MonadPut(..)
    , MonadPut'
    , modifyEnviron
    ) where

import Control.Monad.Reader
import Control.Monad.RWS.Lazy as Lazy
import Control.Monad.RWS.Strict as Strict
import Control.Monad.State.Lazy as Lazy
import Control.Monad.State.Strict as Strict
import Data.Proxy

-- | like 'MonadReader' but with overlapping instances.
-- The 'Proxy' @p@ allows controlled inference of @p m -> r@.
class Monad m => MonadAsk p r m | p m -> r where
    askEnviron :: Proxy p -> m r

type MonadAsk' r = MonadAsk r r

-- | Any transformer on top of 'MonadAsk' is also a 'MonadAsk'
instance {-# OVERLAPPABLE #-} (Monad (t m), MonadTrans t, MonadAsk p r m) => MonadAsk p r (t m) where
    askEnviron = lift . askEnviron

instance {-# OVERLAPPABLE #-} Monad m => MonadAsk r r (ReaderT r m) where
    askEnviron _ = ask

instance {-# OVERLAPPABLE #-} Monad m => MonadAsk s s (Strict.StateT s m) where
    askEnviron _ = get

instance {-# OVERLAPPABLE #-} Monad m => MonadAsk s s (Lazy.StateT s m) where
    askEnviron _ = get

instance {-# OVERLAPPABLE #-} (Monoid w, Monad m) => MonadAsk r r (Strict.RWST r w s m) where
    askEnviron _ = ask

instance {-# OVERLAPPABLE #-} (Monoid w, Monad m) => MonadAsk r r (Lazy.RWST r w s m) where
    askEnviron _ = ask

instance {-# OVERLAPPABLE #-} (Monoid w, Monad m) => MonadAsk s s (Strict.RWST r w s m) where
    askEnviron _ = get

instance {-# OVERLAPPABLE #-} (Monoid w, Monad m) => MonadAsk s s (Lazy.RWST r w s m) where
    askEnviron _ = get

-- | like 'MonadState' but with overlapping instances.
-- The 'Proxy' @p@ allows controlled inference of @p m -> s@.
class MonadAsk p s m => MonadPut p s m | p m -> s where
    putEnviron :: Proxy p -> s -> m ()
    -- getEnviron :: m s
    -- getEnviron = askEnviron (Proxy :: Proxy s)

type MonadPut' s = MonadPut s s

instance {-# OVERLAPPABLE #-} (Monad (t m), MonadTrans t, MonadPut p s m, Monad m) => MonadPut p s (t m) where
    putEnviron p = lift . putEnviron p

instance {-# OVERLAPPABLE #-} (Monad m, MonadAsk p s (Strict.StateT s m)) => MonadPut p s (Strict.StateT s m) where
    putEnviron _ = put

instance {-# OVERLAPPABLE #-} (Monad m, MonadAsk p s (Lazy.StateT s m)) => MonadPut p s (Lazy.StateT s m) where
    putEnviron _ = put

instance {-# OVERLAPPABLE #-} (Monoid w, Monad m, MonadAsk p s (Strict.RWST r w s m)) => MonadPut p s (Strict.RWST r w s m) where
    putEnviron _ = put

instance {-# OVERLAPPABLE #-} (Monoid w, Monad m, MonadAsk p s (Lazy.RWST r w s m)) => MonadPut p s (Lazy.RWST r w s m) where
    putEnviron _ = put

-- | Using ScopedTypeVariables to specify the type of @s@ into 'askEnviron'
modifyEnviron :: forall p s m. MonadPut p s m => Proxy p -> (s -> s) -> m ()
modifyEnviron p f = do
    s <- askEnviron p
    putEnviron p $! f s
