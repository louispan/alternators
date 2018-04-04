{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Monad.Trans.MCont where

import Control.Applicative
import Control.Monad.Cont
import Control.Monad.Fail
import Control.Monad.Reader
import Control.Monad.State.Class
import Control.Monad.Zip
import Control.Newtype
import Data.Functor.Identity
import Data.Semigroup
import qualified GHC.Generics as G

-- | Continuations that threads the actions instead of the values.
newtype MContT r m a = MContT { runMContT :: (m a -> m r) -> m r }
    deriving (G.Generic)

type MCont r = MContT r Identity

mContT' :: Monad m => ((a -> m r) -> m r) -> MContT r m a
mContT' m = MContT $ \k -> m (\a -> k (pure a))

runMContT' :: Monad m => MContT r m a -> (a -> m r) -> m r
runMContT' (MContT m) k = m (\ma -> ma >>= k)

mCont :: ((a -> r) -> r) -> MCont r a
mCont f = MContT (\c -> Identity (f (runIdentity . c . pure)))

runMCont :: MCont r a -> (a -> r) -> r
runMCont (MContT m) k = runIdentity (m (Identity . k . runIdentity))

evalMContT :: MContT r m r -> m r
evalMContT (MContT m) = m id

evalMCont :: MCont r r -> r
evalMCont m = runIdentity (evalMContT m)

mapMContT :: (m r -> m r) -> MContT r m a -> MContT r m a
mapMContT f m = MContT $ f . runMContT m

mapMCont :: (r -> r) -> MCont r a -> MCont r a
mapMCont f = mapMContT (Identity . f . runIdentity)

withMContT :: ((m b -> m r) -> m a -> m r) -> MContT r m a -> MContT r m b
withMContT f (MContT m) = MContT $ m . f

withMContT' :: Monad m => ((b -> m r) -> a -> m r) -> MContT r m a -> MContT r m b
withMContT' f (MContT m) = MContT $ \k -> m $ \ma ->
    ma >>= f (\b -> k (pure b))

withMCont :: ((b -> r) -> a -> r) -> MCont r a -> MCont r b
withMCont f = withMContT (\mbr (Identity a) ->
    Identity (f (\b -> runIdentity (mbr (Identity b))) a))

instance Newtype (MContT r m a)

instance Functor m => Functor (MContT r m) where
    fmap f (MContT m) = MContT $ \k -> m (k . fmap f)

instance Applicative m => Applicative (MContT r m) where
    pure x = MContT ($ pure x)
    f <*> v = MContT $ \c -> runMContT f $ \g -> runMContT v $ \ma -> c (g <*> ma)

instance Monad m => Monad (MContT r m) where
    m >>= k = MContT $ \c -> runMContT m $ \x -> do
        x' <- x
        runMContT (k x') c

instance MonadTrans (MContT r) where
    lift m = MContT ($ m)

instance (MonadIO m) => MonadIO (MContT r m) where
    liftIO = lift . liftIO

instance Monad m => MonadZip (MContT r m) where
    mzip = liftA2 (,)

instance (MonadFail m) => MonadFail (MContT r m) where
    fail msg = MContT $ \ _ -> Control.Monad.Fail.fail msg

instance MonadReader r' m => MonadReader r' (MContT r m) where
    ask = lift ask
    reader = lift . reader
    local f (MContT m) = MContT $ \c -> do
        r <- ask
        local f (m (local (const r) . c))

instance MonadState s m => MonadState s (MContT r m) where
    get = lift get
    put = lift . put
    state = lift . state

-- | This is different from the Alternative/MonadPlus instance.
-- The Alternative/MonadPlus instance runs one or the other
-- The Semigroup/Monoid instances runs both, and fires the output twice.
instance (Semigroup r, Applicative m) => Semigroup (MContT r m c) where
    MContT f <> MContT g =
        MContT $ \k -> liftA2 (<>) (f k) (g k)

-- | This is different from the Alternative/MonadPlus instance.
-- The Alternative/MonadPlus instance runs one or the other
-- The Semigroup/Monoid instances runs both, and fires the output twice.
instance (Monoid r, Applicative m) => Monoid (MContT r m a) where
    mempty = MContT . const $ pure mempty
    MContT f `mappend` MContT g =
        MContT $ \k -> liftA2 mappend (f k) (g k)

instance Alternative m => Alternative (MContT r m) where
    empty = MContT $ const empty
    MContT f <|> MContT g =
        MContT $ \k -> f k <|> g k

instance MonadPlus m => MonadPlus (MContT r m) where
    mzero = empty
    mplus = (<|>)
