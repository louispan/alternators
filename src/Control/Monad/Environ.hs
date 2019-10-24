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

import Control.Monad.Morph
import Control.Monad.Reader
import qualified Control.Monad.RWS.Lazy as Lazy
import qualified Control.Monad.RWS.Strict as Strict
import Control.Monad.State.Class
import qualified Control.Monad.State.Lazy as Lazy
import qualified Control.Monad.State.Strict as Strict
import Control.Monad.Trans.Cont
import Data.Proxy

-- | like 'MonadReader' but with overlapping instances.
-- The 'Proxy' @p@ allows controlled inference of @p m -> r@.
class Monad m => MonadAsk p r m | p m -> r where
    -- | Retrieves the monad environment.
    askEnviron :: Proxy p -> m r
    localEnviron :: Proxy p
          -> (r -> r) -- ^ The function to modify the environment.
          -> m a      -- ^ @Reader@ to run in the modified environment.
          -> m a

type MonadAsk' r = MonadAsk r r

-- | Any transformer on top of 'MonadAsk' is also a 'MonadAsk'
instance {-# OVERLAPPABLE #-} (Monad (t m), MonadTrans t, MFunctor t, MonadAsk p r m) => MonadAsk p r (t m) where
    askEnviron = lift . askEnviron
    localEnviron p f = hoist (localEnviron p f)

-- | ContT is not an instance of MFunctor, so implement this one explicitly
instance {-# OVERLAPPABLE #-} (MonadAsk p r m) => MonadAsk p r (ContT x m) where
    askEnviron = lift . askEnviron
    localEnviron p = liftLocal (askEnviron p) (localEnviron p)

instance {-# OVERLAPPABLE #-} Monad m => MonadAsk r r (ReaderT r m) where
    askEnviron _ = ask
    localEnviron _ = local

instance {-# OVERLAPPABLE #-} Monad m => MonadAsk s s (Strict.StateT s m) where
    askEnviron _ = get
    localEnviron _ = Strict.withStateT

instance {-# OVERLAPPABLE #-} Monad m => MonadAsk s s (Lazy.StateT s m) where
    askEnviron _ = get
    localEnviron _ = Lazy.withStateT

instance {-# OVERLAPPABLE #-} (Monoid w, Monad m) => MonadAsk r r (Strict.RWST r w s m) where
    askEnviron _ = ask
    localEnviron _ = local

instance {-# OVERLAPPABLE #-} (Monoid w, Monad m) => MonadAsk r r (Lazy.RWST r w s m) where
    askEnviron _ = ask
    localEnviron _ = local

instance {-# OVERLAPPABLE #-} (Monoid w, Monad m) => MonadAsk s s (Strict.RWST r w s m) where
    askEnviron _ = get
    localEnviron _ f = Strict.withRWST (\r s -> (r, f s))

instance {-# OVERLAPPABLE #-} (Monoid w, Monad m) => MonadAsk s s (Lazy.RWST r w s m) where
    askEnviron _ = get
    localEnviron _ f = Lazy.withRWST (\r s -> (r, f s))

-- | like 'MonadState' but with overlapping instances.
-- The 'Proxy' @p@ allows controlled inference of @p m -> s@.
class MonadAsk p s m => MonadPut p s m | p m -> s where
    putEnviron :: Proxy p -> s -> m ()

type MonadPut' s = MonadPut s s

-- | Requires 'MFunctor' for the 'MonadAsk' transformer instance
instance {-# OVERLAPPABLE #-} (Monad (t m), MonadTrans t, MFunctor t, MonadPut p s m, Monad m) => MonadPut p s (t m) where
    putEnviron p = lift . putEnviron p

-- | ContT is not an instance of MFunctor, so implement this one explicitly
instance {-# OVERLAPPABLE #-} (Monad m, MonadPut p s m) => MonadPut p s (ContT x m) where
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
