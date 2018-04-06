{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Monad.Trans.Concur where

import Control.Applicative
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Monad.Cont
import Control.Monad.Fail
import Control.Monad.Reader
import Control.Monad.State.Class
import Control.Monad.Zip
import Data.Functor.Identity
import Data.Semigroup

-- | Threads forked STM TMVars commands around continuations
-- so that the Applicative instance will create forked commands in parallel
newtype ConcurT n r m a = ConcurT
    { runConcurT ::
        -- create a noop command
        (() -> m r)
        -- create a TMVar/MVar command
        -> (forall b d. n b -> (b -> m d) -> m d)
        -- continuation that reads from the TMVar/MVar
        -> (n a -> m r)
        -> m r
    }

type Concur n r = ConcurT n r Identity

-- | Thread forked STM TMVar around a continuation
concurTMVarT ::
    (Applicative m, Semigroup r)
    => ((a -> m r) -> m r) -> ConcurT STM r m a
concurTMVarT m = ConcurT $ \noop fork fire ->
    fork newEmptyTMVar $ \v ->
        let x = m $ \a -> fork (putTMVar v a) noop
            y = fire (readTMVar v)
        in liftA2 (<>) x y

concurTMVar ::
    (Semigroup r)
    => ((a -> r) -> r) -> Concur STM r a
concurTMVar m = concurTMVarT (\k -> Identity (m (runIdentity . k)))

-- | Thread forked MVar around a continuation
concurMVarT ::
    (Applicative m, Semigroup r)
    => ((a -> m r) -> m r) -> ConcurT IO r m a
concurMVarT m = ConcurT $ \noop fork fire ->
    fork newEmptyMVar $ \v ->
        let x = m $ \a -> fork (putMVar v a) noop
            y = fire (readMVar v)
        in liftA2 (<>) x y

-- | Thread forked MVar around a continuation
concurMVar ::
    (Semigroup r)
    => ((a -> r) -> r) -> Concur IO r a
concurMVar m = concurMVarT (\k -> Identity (m (runIdentity . k)))

runConcur ::
    Concur n r a
    -> (() -> r)
    -> (forall b d. n b -> (b -> d) -> d)
    -> (n a -> r)
    -> r
runConcur (ConcurT m) noop fork k = runIdentity $ m
    (Identity . noop)
    (\n c -> Identity $ fork n (runIdentity . c))
    (Identity . k)

evalConcurT ::
    (Applicative m)
    => ConcurT n r m r
    -> (() -> m r)
    -> (forall b d. n b -> (b -> m d) -> m d)
    -> m r
evalConcurT (ConcurT m) noop fork = m noop fork (\n -> fork n pure)

evalConcur ::
    Concur n r r
    -> (() -> r)
    -> (forall b d. n b -> (b -> d) -> d)
    -> r
evalConcur m noop fork = runConcur m noop fork (\n -> fork n id)

mapConcurT :: (m r -> m r) -> ConcurT n r m a -> ConcurT n r m a
mapConcurT f (ConcurT m) = ConcurT $ \noop fork k -> m noop fork (f . k)

mapConcur :: (r -> r) -> Concur n r a -> Concur n r a
mapConcur f = mapConcurT (Identity . f . runIdentity)

withConcurT :: ((n b -> m r) -> n a -> m r) -> ConcurT n r m a -> ConcurT n r m b
withConcurT f (ConcurT m) = ConcurT $ \noop fork k -> m noop fork (f k)

withConcur :: ((n b -> r) -> n a -> r) -> Concur n r a -> Concur n r b
withConcur f = withConcurT (\k a -> Identity $ f (runIdentity . k) a)

withConcurT' :: Applicative n => ((b -> m r) -> a -> m r) -> ConcurT n r m a -> ConcurT n r m b
withConcurT' f (ConcurT m) = ConcurT $ \noop fork k ->
    let f' c na = fork na (\a -> f (c . pure) a)
    in m noop fork (f' k)

withConcur' :: Applicative n => ((b -> r) -> a -> r) -> Concur n r a -> Concur n r b
withConcur' f (ConcurT m) = ConcurT $ \noop fork k ->
    let g c a = Identity $ f (runIdentity . c) a
        f' c na = fork na (\a -> g (c . pure) a)
    in m noop fork (f' k)

askNoopT :: Applicative n => ConcurT n r m (() -> m r)
askNoopT = ConcurT $ \noop _ k -> k (pure noop)

askNoop :: Applicative n => Concur n r (() -> r)
askNoop = ConcurT $ \noop _ k -> k (pure (runIdentity . noop))

askForkT :: Applicative n => ConcurT n r m (n b -> (b -> m d) -> m d)
askForkT = ConcurT $ \_ fork k -> k (pure fork)

askFork :: Applicative n => Concur n r (n b -> (b -> d) -> d)
askFork = ConcurT $ \_ fork k -> k (pure (\n c -> runIdentity $ fork n (Identity . c)))

instance Functor n => Functor (ConcurT n r m) where
    fmap f (ConcurT m) = ConcurT $ \noop fork k -> m noop fork (k . fmap f)

instance Applicative n => Applicative (ConcurT n r m) where
    pure x = ConcurT $ \_ _ k -> k $ pure x
    f <*> v = ConcurT $ \noop fork c -> runConcurT f noop fork
        $ \g -> runConcurT v noop fork $ \ma -> c (g <*> ma)

instance (Monad n, Monad m) => Monad (ConcurT n r m) where
    m >>= k = ConcurT $ \noop fork c -> runConcurT m noop fork $ \x -> do
        x' <- fork x pure
        runConcurT (k x') noop fork c

instance Applicative n => MonadTrans (ConcurT n r) where
    lift m = ConcurT $ \_ _ k -> do
        r <- m
        k (pure r)

instance (MonadIO m, Monad n) => MonadIO (ConcurT n r m) where
    liftIO = lift . liftIO

instance (Monad m, Monad n) => MonadZip (ConcurT n r m) where
    mzip = liftA2 (,)

instance (MonadFail m, Monad n) => MonadFail (ConcurT n r m) where
    fail msg = ConcurT $ \ _ _ _ -> Control.Monad.Fail.fail msg

instance (MonadReader r' m, Monad n) => MonadReader r' (ConcurT n r m) where
    ask = lift ask
    reader = lift . reader
    local f (ConcurT m) = ConcurT $ \noop fork c -> do
        r <- ask
        local f (m noop fork (local (const r) . c))

instance (MonadState s m, Monad n) => MonadState s (ConcurT n r m) where
    get = lift get
    put = lift . put
    state = lift . state

-- | This is different from the Alternative/MonadPlus instance.
-- The Alternative/MonadPlus instance runs one or the other
-- The Semigroup/Monoid instances runs both, and fires the output twice.
instance (Monoid r, Applicative m) => Monoid (ConcurT n r m a) where
    mempty = ConcurT $ \_ _ _ -> pure mempty
    ConcurT f `mappend` ConcurT g =
        ConcurT $ \noop fork k -> liftA2 mappend (f noop fork k) (g noop fork k)

instance (Alternative m, Monad n) => Alternative (ConcurT n r m) where
    empty = ConcurT $ \_ _ _ -> empty
    ConcurT f <|> ConcurT g =
        ConcurT $ \noop fork k -> f noop fork k <|> g noop fork k

instance (MonadPlus m, Monad n) => MonadPlus (ConcurT n r m) where
    mzero = empty
    mplus = (<|>)
