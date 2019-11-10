{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

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
-- Law:
-- @
-- id = \m -> delegate $ \fire -> m `dischargedBy` fire
-- @
class MonadCont m => MonadDelegate m where

    -- | Delegates the handing of @a@ to a continuation.
    -- The intuition is that delegate allows deferring subsequent binds
    -- to code defined later.
    -- The "inverse" of 'delegate' is 'dischargedBy' which looks is a bit like a 'bind'.
    delegate :: ((a -> m ()) -> m ()) -> m a

    -- | This signature looks a bit like 'bind'.
    -- Apply handler to @m a @ and result in a monad that will
    -- fire @()@ at most once.
    -- Therefore, subsequent 'bind' will be guaranteed to run at most once.
    -- Each time the input monad is handled, it is is reduced to @m ()@
    -- and then 'bind'ed to the next handing of the input monad.
    -- That is, the input monadis fired and handled one after the other.
    dischargedBy :: m a -> (a -> m ()) -> m ()

infixl 1 `dischargedBy` -- like `(>>=)`
infixr 1 `discharges` -- like `(=<<)`

discharges :: MonadDelegate m => (a -> m ()) -> m a -> m ()
discharges = flip dischargedBy

-- | Instance that does real work using continuations
instance Monad m => MonadDelegate (ContT () m) where
    delegate f = ContT $ \k -> evalContT $ f (lift . k)
    dischargedBy (ContT g) f = lift $ g (evalContT . f)
    -- id m = delegate $ \fire -> m `dischargedBy` fire
    -- id m `should equal` ContT $ \k -> m' k
    -- m' = runContT m :: (a -> m ()) -> m ()
    -- ContT $ \k -> evalContT $ f (lift . k)
    -- k :: a -> m ())
    -- f = lift $ m' (evalContT . fire)
    -- fire = (lift . k) -- lift from m () -> ContT () m ()
    -- fire :: a -> ContT () m ()
    -- ContT $ \k -> evalContT $ lift $ m' (evalContT . lift . k)
    -- ContT (\z -> m >>= z)
    -- evalContT . lift = id
    -- ContT $ \k -> evalContT $ lift $ m' (id . k)
    -- ContT $ \k -> evalContT $ lift $ m' k
    -- ContT $ \k -> m' k
    -- `equals` id m

-- | Passthrough instance
instance (MonadDelegate m) => MonadDelegate (IdentityT m) where
    delegate f = IdentityT $ delegate $ \k -> runIdentityT $ f (lift . k)
    dischargedBy (IdentityT g) f = lift $ g `dischargedBy` (runIdentityT . f)

-- | Passthrough instance
instance (MonadDelegate m) => MonadDelegate (ReaderT env m) where
    delegate f = ReaderT $ \env -> delegate $ \k -> (`runReaderT` env) $ f (lift . k)
    dischargedBy (ReaderT g) f = ReaderT $ \r -> (g r) `dischargedBy` ((`runReaderT` r) . f)

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

    -- dischargedBy :: MaybeT m a -> (a -> MaybeT m ()) -> MaybeT m ()
    -- f :: (a -> MaybeT m ()
    dischargedBy (MaybeT g) f = MaybeT $ do
        -- Make sure we only call fire once
        -- callCC :: ((Maybe () -> m ()) -> m (Maybe ()) -> m (Maybe ())
        -- dischargedBy :: m (Maybe a) -> (Maybe a -> m ()) -> m ()
        callCC $ \escape -> Just <$> dischargedBy g (f' escape)
      where
        -- f' :: Maybe a -> m ()
        f' escape Nothing = escape Nothing
        f' escape (Just a) = go escape $ f a
        -- go :: (Maybe () -> m (Maybe ())) -> MaybeT m () -> m ()
        go escape (MaybeT m) = do
            ma <- m
            case ma of
                Nothing -> escape Nothing -- need to prevent subsequent fires
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

    dischargedBy (ExceptT g) f = ExceptT $ do
        -- Make sure we only call fire once
        callCC $ \escape -> Right <$> dischargedBy g (f' escape)
      where
        f' escape (Left e) = escape (Left e)
        f' escape (Right a) = go escape $ f a
        go escape (ExceptT m) = do
            ma <- m
            case ma of
                (Left e) -> escape (Left e) -- need to prevent subsequent fires
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
