{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}

module Control.Monad.Delegate where

import Control.Applicative
import Control.Monad.Cont
import Control.Monad.Trans.Cont (evalContT)
import Control.Monad.Trans.Except
import Control.Monad.Trans.Identity
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader

-- | A monad for firing and handling events
-- @MonadDelegate m => m a@ is a monad that may fire an event @a@ zero, once or many times.
-- and monadic binding with @a -> m b@ is the body of the for loop that handles the @a@ t
-- for each time the @a@ is fires.
--
-- A 'MonadDelegate' usually requires a 'ContT' or 'Control.Monad.AContT' in the transformer stack.
-- Consider using a @AContT r (MaybeT m)@ in your transformer stack as it allows an 'Alternative' instance.
-- which allows 'dischargeHead'
--
-- Applicative instance has the following semantics:
-- @pure "foo"@ is a producer that produces "foo" once
--
-- Alternative instance of MonadDelegate must follow the following semantics:
-- 'empty' is a producer that never produces anything (Nothing)
-- 'empty' can also be used after firing some events to mean "continue" with next
-- Alternative producer.
-- @empty *> m = empty@ ie, anything after an empty is ignored.
--
-- left <|> right means fire from left but catch any 'empty' with 'right'


class (Monad m) => MonadDelegate m where
    -- | Delegates the handing of @a@ to a continuation.
    -- The intuition is that delegate allows deferring subsequent binds
    -- to code defined later.
    -- The "inverse" of 'delegate' is 'dischargeBy' which looks is a bit like a 'bind'.
    delegate :: ((a -> m ()) -> m ()) -> m a

    -- | Given a terminate monad that signals stop where
    -- terminate is stronger than 'empty' and 'also'
    -- in that @terminate <|> pure () = terminate@
    -- and @terminate `also` pure () = terminate@
    -- return a monad that doesn't have the terminate marker
    terminally :: (m b -> m a) -> m a


-- | The dual to  'delegate'.
-- Using 'delegate' results in a monad that may fire zero, once, or many times
-- Using 'discharge' results in a monad that is guaranteed to fire unit once.
--
-- Law:
-- @
-- m = delegate $ \fire -> m `dischargeBy` fire
-- @
class Monad m => MonadDischarge m where
    -- | This signature looks a bit like 'flip' 'bind'.
    -- Apply handler to @m a @ and result in a monad that will
    -- fire @()@ at most once.
    -- The input monad is fired and handled one after the other.
    -- as if the input monad is reduced to @m ()@
    -- and then 'bind'ed to the next handing of the input monad.
    -- This results in a monad that is guaranteed to fire unit once.
    discharge :: (a -> m ()) -> m a -> m ()

-- class (Alternative m, MonadDelegate m) => MonadTerminate m where
--     -- stop firing events. This is stronger than 'empty'
--     -- in that @terminate <|> pure () = terminate@
--     -- Semantically, it is a bit like "break" in C.
--     terminate :: m a

infixl 1 `dischargeBy` -- like `(>>=)`
infixr 1 `discharge` -- like `(=<<)`

dischargeBy :: MonadDischarge m => m a -> (a -> m ()) -> m ()
dischargeBy = flip discharge

-- | A binary associatve function with (pure ()) as the zero.
-- 'also' drains the left producer, followed by draining the right producer.
also :: (MonadDelegate m, MonadDischarge m) => m a -> m a -> m a
f `also` g = delegate $ \fire -> do
    fire `discharge` f
    fire `discharge` g

-- | convert to something that will fire its events as a Just
-- followed by a final Nothing
withEnding :: (MonadDelegate m, MonadDischarge m, Alternative m) => m a -> m (Maybe a)
-- FIXME: wrong, only want to add empty IFF NOT already empty
withEnding m = (Just <$> (m `also` empty)) <|> pure Nothing


-- | Convert a monad that fires multiple times, to a most that
-- at most fires once.
delegateHead :: (MonadDelegate m) => m a -> m a
delegateHead m = do
    terminally $ \terminate -> delegate $ \fire -> do
        -- for every event, fire it, then stop
        -- which means stop after first event
        a <- m
        fire a
        terminate

-- | Convert a monad that fires multiple times, to a most that
-- at most fires once.
delegateHeadIO :: (MonadIO m, MonadDelegate m) => m a -> m a
delegateHeadIO m = do
    terminally $ \terminate -> delegate $ \fire -> do
        -- for every event, fire it, then stop
        -- which means stop after first event
        liftIO $ putStrLn "head 1 about to run"
        a <- m
        liftIO $ putStrLn "head 2 about to fire"
        fire a
        liftIO $ putStrLn "head 3 fired"
        void $ terminate
        liftIO $ putStrLn "head 4"

-- -- | collect all the times the input monad fires into a list.
-- -- Does not fire an empty list.
-- dischargeList :: (MonadDelegate m, MonadIO m) => m a -> m [a]
-- dischargeList m = do
--     v <- liftIO $ newIORef (DL.empty)
--     let f a = liftIO $ atomicModifyIORef v (\b -> (b `DL.snoc` a, ()))
--     discharge f m
--     liftIO $ DL.toList <$> readIORef v


-- | Instance that does real work using continuations
instance Monad m => MonadDelegate (ContT () (MaybeT m)) where
    terminally f = unterminate $ f terminate
      where
        terminate = ContT $ \_ -> empty
        unterminate (ContT g) = ContT $ \k -> lift $ void $ runMaybeT $ g k
    delegate f = ContT $ \k -> evalContT $ f (lift . k)

instance Monad m => MonadDischarge (ContT () (MaybeT m)) where
    discharge f (ContT g) = lift $ g (evalContT . f)

-- | Passthrough instance
-- instance (MonadTerminate m) => MonadTerminate (IdentityT m) where
    -- terminally (IdentityT m) = lift $ terminally m
    -- (IdentityT f) `also` (IdentityT g) = IdentityT $ f `also` g

instance (MonadDelegate m) => MonadDelegate (IdentityT m) where
    terminally f = IdentityT $ terminally $ \terminate -> runIdentityT $ f (lift terminate)
    delegate f = IdentityT $ delegate $ \k -> runIdentityT $ f (lift . k)

instance (MonadDischarge m) => MonadDischarge (IdentityT m) where
    discharge f (IdentityT m) = lift $ (runIdentityT . f) `discharge` m

-- | Passthrough instance
-- instance (MonadTerminate m) => MonadTerminate (ReaderT env m) where
--     terminally (ReaderT f) = ReaderT $ \r -> terminally (f r)
--     (ReaderT f) `also` (ReaderT g) = ReaderT $ \r -> f r `also` g r

instance (MonadDelegate m) => MonadDelegate (ReaderT env m) where
    terminally f = ReaderT $ \r -> terminally $ \terminate -> (`runReaderT` r) $ f (lift terminate)
    delegate f = ReaderT $ \env -> delegate $ \k -> (`runReaderT` env) $ f (lift . k)

instance (MonadDischarge m) => MonadDischarge (ReaderT env m) where
    discharge f (ReaderT g) = ReaderT $ \r -> ((`runReaderT` r) . f) `discharge` (g r)

-- instance (MonadTerminate m) => MonadTerminate (MaybeT m) where
--     terminally (MaybeT m) = MaybeT $ terminally m
--     (MaybeT f) `also` (MaybeT g) = MaybeT $ f `also` g

instance (MonadDelegate m) => MonadDelegate (MaybeT m) where
    terminally f = MaybeT $ terminally $ \terminate -> runMaybeT $ f (lift terminate)
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

instance Monad m => MonadDischarge (MaybeT (ContT () (MaybeT m))) where
    discharge f (MaybeT (ContT g)) =
            -- g :: (Maybe a -> MaybeT m ()) -> MaybeT m ()
            -- f :: a -> MaybeT (ContT () (MaybeT m))
            -- k :: (Maybe () -> MaybeT m ())
            MaybeT $ ContT $ \k -> lift (runMaybeT (g f')) >>= k

      where
        -- f' :: Maybe a -> MaybeT m ()
        f' Nothing = empty
        -- f a :: MaybeT (ContT () (MaybeT m))
        -- runMaybeT (f a) :: ContT () (MaybeT m) (Maybe ())
        -- runContT (runMaybeT (f a)) :: (Maybe () -> MaybeT m ()) -> MaybeT m ()
        f' (Just a) = runContT ((runMaybeT (f a))) (MaybeT . pure)

-- instance (MonadDischarge m, MonadDelegate m, MonadCont m) => MonadDischarge (MaybeT m) where
--     -- dischargedBy :: MaybeT m a -> (a -> MaybeT m ()) -> MaybeT m ()
--     -- f :: (a -> MaybeT m () or (a -> m (Maybe ())
--     discharge f (MaybeT g) =
--             -- callCC f = ContT $ \ c -> runContT (f (\ x -> ContT $ \ _ -> c x)) c
--             -- callCC :: ((Maybe a -> m ()) -> m (Maybe a)) -> m (Maybe a)
--             -- callCC :: ((a -> MaybeT m ()) -> MaybeT m a) -> MaybeT m a

--             -- fire :: Maybe () -> m ()
--             MaybeT $
--                     ma <- delegate $ \fire -> do
--                         dischargeBy g (f' fire)
--                         fire $ Just ()
--                     case ma of
--                         Nothing -> terminate
--                         Just _ -> pure $ Just ()

--       where
--         -- f' :: Maybe a -> m ()
--         f' fire Nothing = fire Nothing
--         f' fire (Just a) = do
--             ma <- runMaybeT $ f a
--             case ma of
--                 Nothing -> fire Nothing -- need to prevent subsequent fires
--                 -- Just () -> pure ()
--                 _ -> pure ()

-- instance (Monoid e, MonadTerminate m) => MonadTerminate (ExceptT e m) where
--     terminally (ExceptT m) = ExceptT $ f <$> terminally m
--       where
--         f Nothing = Right Nothing
--         f (Just (Left e)) = Left e
--         f (Just (Right e)) = Right (Just e)
--     (ExceptT f) `also` (ExceptT g) = ExceptT $ f `also` g

instance (MonadDelegate m) => MonadDelegate (ExceptT e m) where
    terminally f = ExceptT $ terminally $ \terminate -> runExceptT $ f (lift terminate)
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

instance (MonadCont m, MonadDelegate m, MonadDischarge m) => MonadDischarge (ExceptT e m) where
    discharge f (ExceptT g) =
        ExceptT $ callCC $ \escape -> Right <$> dischargeBy g (f' escape)
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

-- -- | Only handle with given monad, and ignore anything else.
-- -- @forall@ so @TypeApplications@ can be used to specify the type of @a@.
-- -- It pretends to fire @a@ but never does.
-- -- This means subseqent fmap, aps, binds are always ignored.
-- -- This is like @throw@ that exits event handling to a different control scope.
-- finish :: forall a m. MonadDelegate m => m () -> m a
-- finish = delegate . const

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



