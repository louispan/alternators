{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}

module Control.Monad.Delegate where

import Control.Monad.Cont
import Control.Monad.Trans.Cont (evalContT)
import Control.Monad.Trans.Except
import Control.Monad.Trans.Identity
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader

-- | A monad for firing and handling events
-- @MonadDelegate m => m a@ is a monad that may fire an event @a@ zero, once or many times.
-- and monadic binding with @a -> m b@ is handling the @a@ to firing a @b@ event
-- for each time the @a@ is fires.
--
-- This basically requires a 'ContT' or 'Control.Monad.AContT' in the transformer stack.
-- Consider using a @AContT r (MaybeT m)@ in your transformer stack as it allows an 'Alternative' instance.
class Monad m => MonadDelegate m where

    -- | Delegates the handing of @a@ to a continuation.
    -- The intuition is that delegate allows deferring subsequent binds
    -- to code defined later.
    -- The "inverse" of 'delegate' is 'dischargeBy' which looks is a bit like a 'bind'.
    delegate :: ((a -> m ()) -> m ()) -> m a


-- | Adds in inverse operation to 'delegate'.
-- Using 'delegate' results in a monad that may fire zero, once, or many times
-- Using 'discharge' results in a monad that is guaranteed to fire unit once.
--
-- Law:
-- @
-- m = delegate $ \fire -> m `dischargeBy` fire
-- @
class MonadDelegate m => MonadDischarge m where
    -- | This signature looks a bit like 'flip' 'bind'.
    -- Apply handler to @m a @ and result in a monad that will
    -- fire @()@ at most once.
    -- The input monad is fired and handled one after the other.
    -- as if the input monad is reduced to @m ()@
    -- and then 'bind'ed to the next handing of the input monad.
    -- This results in a monad that is guaranteed to fire unit once.
    discharge :: (a -> m ()) -> m a -> m ()

infixl 1 `dischargeBy` -- like `(>>=)`
infixr 1 `discharge` -- like `(=<<)`

dischargeBy :: MonadDischarge m => m a -> (a -> m ()) -> m ()
dischargeBy = flip discharge

-- -- | collect all the times the input monad fires into a list.
-- -- Does not fire an empty list.
-- dischargeList :: (MonadDelegate m, MonadIO m) => m a -> m [a]
-- dischargeList m = do
--     v <- liftIO $ newIORef (DL.empty)
--     let f a = liftIO $ atomicModifyIORef v (\b -> (b `DL.snoc` a, ()))
--     discharge f m
--     liftIO $ DL.toList <$> readIORef v

-- | Instance that does real work using continuations
instance Monad m => MonadDelegate (ContT () m) where
    delegate f = ContT $ \k -> evalContT $ f (lift . k)

instance Monad m => MonadDischarge (ContT () m) where
    discharge f (ContT g) = lift $ g (evalContT . f)

-- | Passthrough instance
instance (MonadDelegate m) => MonadDelegate (IdentityT m) where
    delegate f = IdentityT $ delegate $ \k -> runIdentityT $ f (lift . k)

instance (MonadDischarge m) => MonadDischarge (IdentityT m) where
    discharge f (IdentityT m) = lift $ (runIdentityT . f) `discharge` m

-- | Passthrough instance
instance (MonadDelegate m) => MonadDelegate (ReaderT env m) where
    delegate f = ReaderT $ \env -> delegate $ \k -> (`runReaderT` env) $ f (lift . k)

instance (MonadDischarge m) => MonadDischarge (ReaderT env m) where
    discharge f (ReaderT g) = ReaderT $ \r -> ((`runReaderT` r) . f) `discharge` (g r)

-- | There is no instance of 'MonadDischarge' for 'MaybeT'
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

-- | There is no instance of 'MonadDischarge' for 'ExceptT'
instance (MonadCont m, MonadDelegate m) => MonadDelegate (ExceptT e m) where
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

-- | 'delegate' handling of two different things
delegate2 :: MonadDelegate m => ((a -> m (), b -> m ()) -> m ()) -> m (Either a b)
delegate2 f = delegate $ \fire -> f (fire . Left, fire . Right)

-- | Only handle with given monad, and ignore anything else.
-- @forall@ so @TypeApplications@ can be used to specify the type of @a@.
-- It pretends to fire @a@ but never does.
-- This means subseqent fmap, aps, binds are always ignored.
-- This is like @throw@ that exits event handling to a different control scope.
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

-- -- | Only care handling the 'Just' case, don't do anything for 'Nothing'
-- onJust :: MonadDelegate m => Maybe a -> m a
-- onJust ma = delegate $ \fire ->
--     case ma of
--         Nothing -> pure ()
--         Just a -> fire a



