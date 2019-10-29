{-# LANGUAGE AllowAmbiguousTypes #-}
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
    , askEnviron
    , askEnviron'
    , askEnvironTagged
    , localEnviron
    , localEnviron'
    , localEnvironTagged
    , MonadPut(..)
    , MonadPut'
    , putEnviron
    , putEnviron'
    , putEnvironTagged
    , modifyEnviron
    , modifyEnviron'
    , modifyEnvironTagged
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
import Data.Tagged.Extras

-- | like 'MonadReader' but with overlapping instances.
-- The 'Proxy' @p@ allows controlled inference of @p m -> r@.
class Monad m => MonadAsk p r m | p m -> r where
    -- | Retrieves the monad environment.
    askEnvironP :: Proxy p -> m r
    localEnvironP :: Proxy p
          -> (r -> r) -- ^ The function to modify the environment.
          -> m a      -- ^ @Reader@ to run in the modified environment.
          -> m a

type MonadAsk' r = MonadAsk r r

-- | 'askEnvironP' without 'Proxy'.
-- Use with @TypeApplications@ to specify @p@
askEnviron :: forall p r m. MonadAsk p r m => m r
askEnviron = askEnvironP @p Proxy

-- | Convenient version 'askEnvironP' for the simple case where @p@ matches @r@
-- Use with @TypeApplications@ to specify @r@
askEnviron' :: forall r m. MonadAsk' r m => m r
askEnviron' = askEnvironP @r Proxy

-- | Convenient version 'askEnvironP' for the case where @p@ is @Tagged tag r@
-- but the tag is automatically unwrapped when used.
-- Use with @TypeApplications@ to specify @tag@
askEnvironTagged :: forall tag r m. MonadAsk' (Tagged tag r) m => m r
askEnvironTagged = (untag' @tag) <$> askEnvironP @(Tagged tag r) Proxy

-- | 'localEnvironP' without 'Proxy'.
-- Use with @TypeApplications@ to specify @p@
localEnviron :: forall p r a m. MonadAsk p r m => (r -> r) -> m a -> m a
localEnviron = localEnvironP @p Proxy

-- | Convenient version of 'localEnvironP' for the simple case where @p@ matches @r@
-- Use with @TypeApplications@ to specify @r@
localEnviron' :: forall r a m. MonadAsk' r m => (r -> r) -> m a -> m a
localEnviron' = localEnvironP @r Proxy

-- | Convenient version 'localEnvironP' for the case where @p@ is @Tagged tag r@
-- but the tag is automatically unwrapped when used.
-- Use with @TypeApplications@ to specify @tag@
localEnvironTagged :: forall tag r a m. MonadAsk' (Tagged tag r) m => (r -> r) -> m a -> m a
localEnvironTagged f = localEnvironP @(Tagged tag r) Proxy (Tagged @tag . f . untag' @tag)

-- | Any transformer on top of 'MonadAsk' is also a 'MonadAsk'
instance {-# OVERLAPPABLE #-} (Monad (t m), MonadTrans t, MFunctor t, MonadAsk p r m) => MonadAsk p r (t m) where
    askEnvironP = lift . askEnvironP
    localEnvironP p f = hoist (localEnvironP p f)

-- | ContT is not an instance of MFunctor, so implement this one explicitly
instance {-# OVERLAPPABLE #-} (MonadAsk p r m) => MonadAsk p r (ContT x m) where
    askEnvironP = lift . askEnvironP
    localEnvironP p = liftLocal (askEnvironP p) (localEnvironP p)

instance {-# OVERLAPPABLE #-} Monad m => MonadAsk r r (ReaderT r m) where
    askEnvironP _ = ask
    localEnvironP _ = local

instance {-# OVERLAPPABLE #-} Monad m => MonadAsk s s (Strict.StateT s m) where
    askEnvironP _ = get
    localEnvironP _ = Strict.withStateT

instance {-# OVERLAPPABLE #-} Monad m => MonadAsk s s (Lazy.StateT s m) where
    askEnvironP _ = get
    localEnvironP _ = Lazy.withStateT

instance {-# OVERLAPPABLE #-} (Monoid w, Monad m) => MonadAsk r r (Strict.RWST r w s m) where
    askEnvironP _ = ask
    localEnvironP _ = local

instance {-# OVERLAPPABLE #-} (Monoid w, Monad m) => MonadAsk r r (Lazy.RWST r w s m) where
    askEnvironP _ = ask
    localEnvironP _ = local

instance {-# OVERLAPPABLE #-} (Monoid w, Monad m) => MonadAsk s s (Strict.RWST r w s m) where
    askEnvironP _ = get
    localEnvironP _ f = Strict.withRWST (\r s -> (r, f s))

instance {-# OVERLAPPABLE #-} (Monoid w, Monad m) => MonadAsk s s (Lazy.RWST r w s m) where
    askEnvironP _ = get
    localEnvironP _ f = Lazy.withRWST (\r s -> (r, f s))

-- | like 'MonadState' but with overlapping instances.
-- The 'Proxy' @p@ allows controlled inference of @p m -> s@.
class MonadAsk p s m => MonadPut p s m | p m -> s where
    putEnvironP :: Proxy p -> s -> m ()

type MonadPut' s = MonadPut s s

-- | 'putEnvironP' without 'Proxy'.
-- Use with @TypeApplications@ to specify @p@
putEnviron :: forall p s m. MonadPut p s m => s -> m ()
putEnviron = putEnvironP @p Proxy

-- | Convenient version of 'putEnvironP' for the simple case where @p@ matches @r@
putEnviron' :: forall s m. MonadPut' s m => s -> m ()
putEnviron' = putEnvironP @s Proxy

-- | Convenient version 'putEnvironP' for the case where @p@ is @Tagged tag r@
-- but the tag is automatically unwrapped when used.
-- Use with @TypeApplications@ to specify @tag@
putEnvironTagged :: forall tag s m. MonadPut' (Tagged tag s) m => s -> m ()
putEnvironTagged s = putEnvironP @(Tagged tag s) Proxy (Tagged @tag s)

-- | Requires 'MFunctor' for the 'MonadAsk' transformer instance
instance {-# OVERLAPPABLE #-} (Monad (t m), MonadTrans t, MFunctor t, MonadPut p s m, Monad m) => MonadPut p s (t m) where
    putEnvironP p = lift . putEnvironP p

-- | ContT is not an instance of MFunctor, so implement this one explicitly
instance {-# OVERLAPPABLE #-} (Monad m, MonadPut p s m) => MonadPut p s (ContT x m) where
    putEnvironP p = lift . putEnvironP p

instance {-# OVERLAPPABLE #-} (Monad m, MonadAsk s s (Strict.StateT s m)) => MonadPut s s (Strict.StateT s m) where
    putEnvironP _ = put

instance {-# OVERLAPPABLE #-} (Monad m, MonadAsk s s (Lazy.StateT s m)) => MonadPut s s (Lazy.StateT s m) where
    putEnvironP _ = put

instance {-# OVERLAPPABLE #-} (Monoid w, Monad m, MonadAsk s s (Strict.RWST r w s m)) => MonadPut s s (Strict.RWST r w s m) where
    putEnvironP _ = put

instance {-# OVERLAPPABLE #-} (Monoid w, Monad m, MonadAsk s s (Lazy.RWST r w s m)) => MonadPut s s (Lazy.RWST r w s m) where
    putEnvironP _ = put

modifyEnvironP :: forall p s m. MonadPut p s m => Proxy p -> (s -> s) -> m ()
modifyEnvironP p f = do
    s <- askEnvironP p
    putEnvironP p $! f s

-- | 'modifyEnvironP' without 'Proxy'.
-- Use with @TypeApplications@ to specify @p@
modifyEnviron :: forall p s m. MonadPut p s m => (s -> s) -> m ()
modifyEnviron = modifyEnvironP @p Proxy

-- | Convenient version of 'putEnviron' for the simple case where @p@ matches @r@
modifyEnviron' :: forall s m. MonadPut' s m => (s -> s) -> m ()
modifyEnviron' = modifyEnvironP @s Proxy

-- | Convenient version 'modifyEnvironP' for the case where @p@ is @Tagged tag r@
-- but the tag is automatically unwrapped when used.
-- Use with @TypeApplications@ to specify @tag@
modifyEnvironTagged :: forall tag s m. MonadPut' (Tagged tag s) m => (s -> s) -> m ()
modifyEnvironTagged f = modifyEnvironP @(Tagged tag s) Proxy (Tagged @tag . f . untag' @tag)
