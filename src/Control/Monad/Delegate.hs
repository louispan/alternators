{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module Control.Monad.Delegate where

import Control.Applicative
import Control.Monad.Cont
import Control.Monad.ST.Class
import Control.Monad.Trans.Cont (evalContT)
import Control.Monad.Trans.Except
import Control.Monad.Trans.Identity
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
import qualified Data.DList as DL
import Data.STRef

-- | A monad for firing and handling events
-- @MonadDelegate m => m a@ is a monad that may fire an event @a@ zero, once or many times.
-- and monadic binding with @a -> m b@ is the body of the for loop that handles the @a@ t
-- for each time the @a@ is fires.
--
-- A 'MonadDelegate' usually requires a 'ContT' or 'Control.Monad.AContT' in the transformer stack.
-- Consider using a @AContT r (MaybeT m)@ in your transformer stack as it allows an 'Alternative' instance.
-- which allows 'terminally' for detecting if an event is fired zero times.
--
-- Applicative instance has the following semantics:
-- @pure x@ is a producer that fires @x@ once
--
-- Alternative instance of MonadDelegate must follow the following semantics:
--
-- left <|> right means only fire from left but catch any 'empty' with right producer.
-- That is, 'empty' mean "catch and continue" with next Alternative producer.
--
-- 'empty' is different from 'finish', although they almost behave the same.
-- They both result in a monad which cannot be bound to.
-- However, ,'finish' is monad that never returns and cannot be caught with '<|>'
-- whereas 'empty' can be caught with '<|>'.
--
-- Use 'terminally' to convert any monad (whether terminated with 'finish' or 'empty') to fire final 'Nothing' event.
class (Monad m) => MonadDelegate m where
    -- | Delegates the handing of @a@ to a continuation.
    -- The intuition is that delegate allows deferring subsequent binds
    -- to code defined later.
    -- The "inverse" of 'delegate' is 'discharge' which looks is a bit like a 'bind'.
    delegate :: ((a -> m ()) -> m ()) -> m a

-- | The dual to  'delegate'.
-- Using 'delegate' results in a monad that may fire zero, once, or many times
-- Using 'discharge' results in a monad that is guaranteed to fire unit once.
--
-- Law:
-- @
-- m = delegate $ discharge m
-- @
class MonadDelegate m => MonadDischarge m where
    -- | This signature looks a bit like constrained 'bind'.
    -- Apply handler to @m a @ and result in a monad that will
    -- fire unit @()@ at most once.
    -- The input monad is fired and handled one after the other.
    -- as if the input monad is reduced to @m ()@
    -- and then 'bind'ed to the next handing of the input monad.
    -- This results in a monad that is guaranteed to fire unit at most once
    -- even if the monad was 'finish'ed.
    -- (The final @m ()@ could still be 'empty')
    discharge :: m a -> (a -> m ()) -> m ()

infixl 1 `discharge` -- like `(>>=)`

instance (MonadDelegate m) => MonadDelegate (IdentityT m) where
    delegate f = IdentityT $ delegate $ \k -> runIdentityT $ f (lift . k)

instance (MonadDischarge m) => MonadDischarge (IdentityT m) where
    discharge (IdentityT m) f = lift $ discharge m (runIdentityT . f)

instance (MonadDelegate m) => MonadDelegate (ReaderT env m) where
    delegate f = ReaderT $ \env -> delegate $ \k -> (`runReaderT` env) $ f (lift . k)

instance (MonadDischarge m) => MonadDischarge (ReaderT env m) where
    discharge (ReaderT g) f = ReaderT $ \r -> discharge (g r) ((`runReaderT` r) . f)

-- | Instance that does real work using continuations
instance Monad m => MonadDelegate (ContT () m) where
    delegate f = ContT $ \k -> evalContT $ f (lift . k)

instance Monad m => MonadDischarge (ContT () m) where
    discharge (ContT g) f = lift $ g (evalContT . f)

-- | There is no instance of 'MonadDischarge' for 'MaybeT'
instance (MonadDelegate m) => MonadDelegate (MaybeT m) where
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
instance (MonadDelegate m) => MonadDelegate (ExceptT e m) where
    -- terminally f = ExceptT $ terminally $ \terminate -> runExceptT $ f (lift terminate)
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

-- | Pretends to fire @a@ but never does.
-- @forall@ so @TypeApplications@ can be used to specify the type of @a@.
-- This means subseqent fmap, aps, binds are always ignored.
-- This is like @throw@ that exits event handling to a different control scope.
finish :: forall a m. MonadDelegate m => m a
finish = delegate . const $ pure ()

-- | A binary associatve function with `finish` as the zero.
-- 'also' drains the left producer, followed by draining the right producer
-- (unless drainig the left producer results in empty)
also :: (MonadDischarge m) => m a -> m a -> m a
f `also` g = delegate $ \fire -> do
    discharge f fire
    discharge g fire

-- | convert any monad (whether terminated with 'finish' or 'empty') its events as a Just
-- followed by a final Nothing.
terminally :: (MonadDischarge m, Alternative m) => m a -> m (Maybe a)
terminally m = (Just <$> (m `also` empty)) <|> pure Nothing

-- | Convert a monad that fires multiple times to something that might fire at most once.
-- A Just or Nothing if nothing was fired.
-- If the 'discharge' monad results in 'empty', then 'dischargeHead' also results in empty
dischargeHead :: (MonadST m, MonadDischarge m) => m a -> m (Maybe a)
dischargeHead m = do
    v <- liftST $ newSTRef Nothing
    discharge m $ go v
    liftST $ readSTRef v
  where
    go v a = do
        v' <- liftST $ readSTRef v
        case v' of
            Nothing -> liftST $ writeSTRef v (Just a)
            Just _ -> pure ()

-- | Collect all the times the monad fires @a@ into a list
-- The result monad is guaranteed to fire @a@ most once (may fire nil list)
-- If the 'discharge' monad results in 'empty',
-- then 'dischargeList' also results in empty
dischargeList :: (MonadST m, MonadDischarge m) => m a -> m [a]
dischargeList m = do
    v <- liftST $ newSTRef DL.empty
    discharge m $ \a -> liftST $ modifySTRef' v (`DL.snoc` a)
    liftST $ DL.toList <$> readSTRef v

