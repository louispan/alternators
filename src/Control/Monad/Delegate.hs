{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Monad.Delegate where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Cont
import Control.Monad.Trans.Except
import Control.Monad.Trans.Identity
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader

-- | Delegates the handing of @a@ to a continuation.
-- The "inverse" of 'delegate' is a bit like a contrained 'bind'
--
-- @
-- (>>=) :: Monad m => m a -> (a -> m b) -> m b
-- contrainedBind :: Monad m => m a -> (a -> m ()) -> m ()
-- @
class Monad m => MonadDelegate m where
    delegate :: ((a -> m ()) -> m ()) -> m a

-- | Instance that does real work using continuations
instance Monad m => MonadDelegate (ContT () m) where
    delegate f = ContT $ \k -> evalContT $ f (lift . k)

-- | Passthrough instance
instance (MonadDelegate m) => MonadDelegate (IdentityT m) where
    delegate f = IdentityT $ delegate $ \k -> runIdentityT $ f (lift . k)

-- | Passthrough instance
instance (MonadDelegate m) => MonadDelegate (ReaderT env m) where
    delegate f = ReaderT $ \env -> delegate $ \k -> (`runReaderT` env) $ f (lift . k)

-- | Passthrough instance
instance (MonadDelegate m) => MonadDelegate (MaybeT m) where
    -- f :: ((a -> MaybeT m ()) -> MaybeT m ())
    delegate f = MaybeT . delegate $ \k -> do -- k :: (Maybe a -> m ())
        -- Use the given @f@ handler with the 'Just' case of @k@
        mr <- runMaybeT . f $ lift . k . Just -- m (Maybe ())
        -- m ()
        case mr of
            -- use the 'Nothing' case of @k@
            Nothing -> k Nothing
            -- Just () -> pure ()
            _ -> pure ()

-- | Passthrough instance
instance (MonadDelegate m) => MonadDelegate (ExceptT e m) where
    -- f :: ((a -> ExceptT e m ()) -> ExceptT e m ())
    delegate f = ExceptT . delegate $ \k -> do -- k :: (Either e a -> m ())
        -- Use the given @f@ handler with the 'Right' case of @k@
        er <- runExceptT . f $ lift . k . Right -- m (Either e ())
        -- m ()
        case er of
            -- use the 'Left' case of @k@
            Left e -> k (Left e)
            -- Right () -> pure ()
            _ -> pure ()

-- | Only handle with given monad, and ignore anything else.
-- This means subseqent fmap, aps, binds are always ignored.
-- @forall@ so @TypeApplications@ can be used to specify the type of @a@
finish :: forall a m. MonadDelegate m => m () -> m a
finish = delegate . const

-- -- | Convert two handler to a monad that may fire two possibilities
-- -- The inverse is 'bindBoth'.
-- multitask :: MonadDelegate r m => ((a -> m r) -> (b -> m r) -> m r) -> m (Either a b)
-- multitask g = delegate $ \fab -> g (fab . Left) (fab . Right)

-- -- | Convert a monad that fires two possibilites to a two handlers.
-- bindBoth :: Monad m => m (Either a b) -> (a -> m r) -> (b -> m r) -> m r
-- bindBoth m fa fb = m >>= either fa fb

-- -- | 'bind' only the 'Right' possibility.
-- bindRight :: Monad m => m (Either a b) -> (b -> m c) -> m (Either a c)
-- bindRight m k = bindBoth m (pure . Left) (fmap Right . k)

-- -- | 'bind' only the 'Left' possibility.
-- bindLeft :: Monad m => m (Either a b) -> (a -> m c) -> m (Either c b)
-- bindLeft m k = bindBoth m (fmap Left . k) (pure . Right)

-- -- | finish the 'Left' possibility
-- finishLeft :: MonadDelegate r m => m (Either r b) -> m b
-- finishLeft m = m >>= either (finish . pure) pure

-- -- | finish the 'Right' possibility
-- finishRight :: MonadDelegate r m => m (Either a r) -> m a
-- finishRight m = m >>= either pure (finish . pure)

-- | Only care handling the 'Just' case, don't do anything for 'Nothing'
onJust :: MonadDelegate m => Maybe a -> m a
onJust ma = delegate $ \fire ->
    case ma of
        Nothing -> pure ()
        Just a -> fire a
