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
    , askEnv
    , askEnv'
    , askTagged
    -- , localEnv
    -- , localEnv'
    -- , localTagged
    , MonadPut(..)
    , MonadPut'
    , putEnv
    , putEnv'
    , putTagged
    , modifyEnv
    , modifyEnv'
    , modifyTagged
    ) where

import Control.Monad.Morph
import Control.Monad.Reader
import qualified Control.Monad.RWS.Lazy as Lazy
import qualified Control.Monad.RWS.Strict as Strict
import Control.Monad.State.Class
import qualified Control.Monad.State.Lazy as Lazy
import qualified Control.Monad.State.Strict as Strict
import Data.Proxy
import Data.Tagged.Extras

-- | like 'MonadReader' but with overlapping instances.
-- The 'Proxy' @p@ allows controlled inference of @p m -> r@.
class Monad m => MonadAsk p r m | p m -> r where
    -- | Retrieves the monad environment.
    askEnvP :: Proxy p -> m r
    -- localEnvP :: Proxy p
    --       -> (r -> r) -- ^ The function to modify the environment.
    --       -> m a      -- ^ @Reader@ to run in the modified environment.
    --       -> m a

type MonadAsk' r = MonadAsk r r

-- | 'askEnvP' without 'Proxy'.
-- Use with @TypeApplications@ to specify @p@
askEnv :: forall p r m. MonadAsk p r m => m r
askEnv = askEnvP @p Proxy

-- | Convenient version 'askEnvP' for the simple case where @p@ matches @r@
-- Use with @TypeApplications@ to specify @r@
askEnv' :: forall r m. MonadAsk' r m => m r
askEnv' = askEnvP @r Proxy

-- | Convenient version 'askEnvP' for the case where @p@ is @Tagged tag r@
-- but the tag is automatically unwrapped when used.
-- Use with @TypeApplications@ to specify @tag@
askTagged :: forall tag r m. MonadAsk' (Tagged tag r) m => m r
askTagged = (untag' @tag) <$> askEnvP @(Tagged tag r) Proxy

-- -- | 'localEnvP' without 'Proxy'.
-- -- Use with @TypeApplications@ to specify @p@
-- localEnv :: forall p r a m. MonadAsk p r m => (r -> r) -> m a -> m a
-- localEnv = localEnvP @p Proxy

-- -- | Convenient version of 'localEnvP' for the simple case where @p@ matches @r@
-- -- Use with @TypeApplications@ to specify @r@
-- localEnv' :: forall r a m. MonadAsk' r m => (r -> r) -> m a -> m a
-- localEnv' = localEnvP @r Proxy

-- -- | Convenient version 'localEnvP' for the case where @p@ is @Tagged tag r@
-- -- but the tag is automatically unwrapped when used.
-- -- Use with @TypeApplications@ to specify @tag@
-- localTagged :: forall tag r a m. MonadAsk' (Tagged tag r) m => (r -> r) -> m a -> m a
-- localTagged f = localEnvP @(Tagged tag r) Proxy (Tagged @tag . f . untag' @tag)

-- | Any transformer on top of 'MonadAsk' is also a 'MonadAsk'
instance {-# OVERLAPPABLE #-} (Monad (t m), MonadTrans t, MonadAsk p r m) => MonadAsk p r (t m) where
    askEnvP = lift . askEnvP
    -- localEnvP p f = hoist (localEnvP p f)

-- -- | ContT is not an instance of MFunctor, so implement this one explicitly
-- instance {-# OVERLAPPABLE #-} (MonadAsk p r m) => MonadAsk p r (ContT x m) where
--     askEnvP = lift . askEnvP
--     -- localEnvP p = Cont.liftLocal (askEnvP p) (localEnvP p)

-- -- | AContT is not an instance of MFunctor, so implement this one explicitly
-- instance {-# OVERLAPPABLE #-} (MonadAsk p r m) => MonadAsk p r (AContT x m) where
--     askEnvP = lift . askEnvP
--     -- localEnvP p = ACont.liftLocal (askEnvP p) (localEnvP p)

instance {-# OVERLAPPABLE #-} Monad m => MonadAsk r r (ReaderT r m) where
    askEnvP _ = ask
    -- localEnvP _ = local

instance {-# OVERLAPPABLE #-} Monad m => MonadAsk s s (Strict.StateT s m) where
    askEnvP _ = get
    -- localEnvP _ = Strict.withStateT

instance {-# OVERLAPPABLE #-} Monad m => MonadAsk s s (Lazy.StateT s m) where
    askEnvP _ = get
    -- localEnvP _ = Lazy.withStateT

instance {-# OVERLAPPABLE #-} (Monoid w, Monad m) => MonadAsk r r (Strict.RWST r w s m) where
    askEnvP _ = ask
    -- localEnvP _ = local

instance {-# OVERLAPPABLE #-} (Monoid w, Monad m) => MonadAsk r r (Lazy.RWST r w s m) where
    askEnvP _ = ask
    -- localEnvP _ = local

instance {-# OVERLAPPABLE #-} (Monoid w, Monad m) => MonadAsk s s (Strict.RWST r w s m) where
    askEnvP _ = get
    -- localEnvP _ f = Strict.withRWST (\r s -> (r, f s))

instance {-# OVERLAPPABLE #-} (Monoid w, Monad m) => MonadAsk s s (Lazy.RWST r w s m) where
    askEnvP _ = get
    -- localEnvP _ f = Lazy.withRWST (\r s -> (r, f s))

-- | like 'MonadState' but with overlapping instances.
-- The 'Proxy' @p@ allows controlled inference of @p m -> s@.
class MonadAsk p s m => MonadPut p s m | p m -> s where
    putEnvP :: Proxy p -> s -> m ()

type MonadPut' s = MonadPut s s

-- | 'putEnvP' without 'Proxy'.
-- Use with @TypeApplications@ to specify @p@
putEnv :: forall p s m. MonadPut p s m => s -> m ()
putEnv = putEnvP @p Proxy

-- | Convenient version of 'putEnvP' for the simple case where @p@ matches @r@
putEnv' :: forall s m. MonadPut' s m => s -> m ()
putEnv' = putEnvP @s Proxy

-- | Convenient version 'putEnvP' for the case where @p@ is @Tagged tag r@
-- but the tag is automatically unwrapped when used.
-- Use with @TypeApplications@ to specify @tag@
putTagged :: forall tag s m. MonadPut' (Tagged tag s) m => s -> m ()
putTagged s = putEnvP @(Tagged tag s) Proxy (Tagged @tag s)

-- | Requires 'MFunctor' for the 'MonadAsk' transformer instance
instance {-# OVERLAPPABLE #-} (Monad (t m), MonadTrans t, MonadPut p s m, Monad m) => MonadPut p s (t m) where
    putEnvP p = lift . putEnvP p

-- -- | ContT is not an instance of MFunctor, so implement this one explicitly
-- instance {-# OVERLAPPABLE #-} (Monad m, MonadPut p s m) => MonadPut p s (ContT x m) where
--     putEnvP p = lift . putEnvP p

-- -- | ContT is not an instance of MFunctor, so implement this one explicitly
-- instance {-# OVERLAPPABLE #-} (Monad m, MonadPut p s m) => MonadPut p s (AContT x m) where
--     putEnvP p = lift . putEnvP p

instance {-# OVERLAPPABLE #-} (Monad m, MonadAsk s s (Strict.StateT s m)) => MonadPut s s (Strict.StateT s m) where
    putEnvP _ = put

instance {-# OVERLAPPABLE #-} (Monad m, MonadAsk s s (Lazy.StateT s m)) => MonadPut s s (Lazy.StateT s m) where
    putEnvP _ = put

instance {-# OVERLAPPABLE #-} (Monoid w, Monad m, MonadAsk s s (Strict.RWST r w s m)) => MonadPut s s (Strict.RWST r w s m) where
    putEnvP _ = put

instance {-# OVERLAPPABLE #-} (Monoid w, Monad m, MonadAsk s s (Lazy.RWST r w s m)) => MonadPut s s (Lazy.RWST r w s m) where
    putEnvP _ = put

modifyEnvP :: forall p s m. MonadPut p s m => Proxy p -> (s -> s) -> m ()
modifyEnvP p f = do
    s <- askEnvP p
    putEnvP p $! f s

-- | 'modifyEnvP' without 'Proxy'.
-- Use with @TypeApplications@ to specify @p@
modifyEnv :: forall p s m. MonadPut p s m => (s -> s) -> m ()
modifyEnv = modifyEnvP @p Proxy

-- | Convenient version of 'putEnv' for the simple case where @p@ matches @r@
modifyEnv' :: forall s m. MonadPut' s m => (s -> s) -> m ()
modifyEnv' = modifyEnvP @s Proxy

-- | Convenient version 'modifyEnvP' for the case where @p@ is @Tagged tag r@
-- but the tag is automatically unwrapped when used.
-- Use with @TypeApplications@ to specify @tag@
modifyTagged :: forall tag s m. MonadPut' (Tagged tag s) m => (s -> s) -> m ()
modifyTagged f = modifyEnvP @(Tagged tag s) Proxy (Tagged @tag . f . untag' @tag)
