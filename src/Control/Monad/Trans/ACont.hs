{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Monad.Trans.ACont where

import Control.Applicative
import Control.Monad.Cont
import Control.Monad.Delegate
import Control.Monad.Morph
import Control.Monad.Trans.Except
import Control.Monad.Trans.Identity
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader

-- -- | The Either Left is True to mean terminate or False to mean empty.
-- newtype TerminateT m a = TerminateT { runTerminateT :: m (Either Bool a) }
--     deriving
--         ( Functor, Applicative, Monad, MonadIO
--         -- , MonadFail
--         ) via (ExceptT Bool m)

-- instance Monad m => Alternative (TerminateT m) where
--     empty = TerminateT (pure $ Left False)
--     TerminateT mx <|> TerminateT my = TerminateT $ do
--         ex <- mx
--         case ex of
--             Left True -> pure (Left True) -- terminate, don't allow anything to continue
--             Left False -> my
--             Right x -> pure (Right x)

-- instance Monad m => MonadTerminally (TerminateT m) where
--     terminally f = TerminateT $ do
--         ex <- runTerminateT $ f terminate
--         case ex of
--             Left _ -> pure (Left False) -- unset terminate
--             Right x -> pure (Right x)
--       where
--         terminate = TerminateT (pure $ Left True)

-- | 'AContT' is a 'ContT' with an 'Alternative' instance.
-- This means @AContT () MaybeT m@ is an instance of 'Alternative', 'MonadDelegate', and 'MonadDischarge'.
--
-- There are at least two possible instances of Alternative for ContT.
-- https://hub.darcs.net/ross/transformers/issue/66
-- https://hub.darcs.net/ross/transformers/issue/69
-- 'AContT' uses the lifted instance https://hub.darcs.net/ross/transformers/issue/66
-- which matches the instance in Control.Monad.Codensity.
newtype AContT r m a = AContT { runAContT :: (a -> m r) -> m r }
    deriving
        ( Functor, Applicative
        , Monad
        , MonadIO
        , MonadCont
        -- , MonadFail
        ) via (ContT r m)

type instance DTransT t (AContT () m) = AContT () (t m)

deriving via (ContT () m) instance Monad m => MonadDelegate (AContT () m)
deriving via (ContT () m) instance Monad m => MonadDischarge (AContT () m)

instance (MonadTrans t, MFunctor t, Monad m) => MonadDTrans t (AContT () m) where
    runDTransT frm (AContT g) = AContT $ \k -> evalAContT . frm . hoist lift . g $ lift . k
    dTransT frm (AContT g) = AContT $ \k -> lift $ g (evalAContT . frm . hoist lift . k)

instance MonadTrans (AContT r) where
    lift m = AContT (m >>=)

instance Alternative m => Alternative (AContT r m) where
    empty = AContT $ \_ -> empty
    (AContT f) <|> (AContT g) = AContT $ \k -> f k <|> g k

#if __GLASGOW_HASKELL__ >= 710
instance Alternative m => MonadPlus (AContT r m)
#else
instance MonadPlus m => MonadPlus (AContT r m) where
    mzero = AContT $ \_ -> mzero
    (AContT f) `mplus` (AContT g) = AContT $ \k -> f k `mplus` g k
#endif

-- instance MonadTerminally m => MonadTerminally (AContT r m) where
--     terminally f = AContT $ \k ->
--         terminally $ \terminate -> (`runAContT` k) $ f (lift terminate)

-- | 'evalContT' for 'AContT'
evalAContT :: (Applicative m) => AContT r m r -> m r
evalAContT m = runAContT m pure

-- | 'withContT' for 'AContT'
withAContT :: ((b -> m r) -> (a -> m r)) -> AContT r m a -> AContT r m b
withAContT f m = AContT $ runAContT m . f

-- | @'liftLocal' ask local@ yields a @local@ function for @'ContT' r m@.
liftLocal :: (Monad m) => m r' -> ((r' -> r') -> m r -> m r) ->
    (r' -> r') -> AContT r m a -> AContT r m a
liftLocal ask local f m = AContT $ \ c -> do
    r <- ask
    local f (runAContT m (local (const r) . c))



-- -- | Inner MaybeT is to signal termination
-- -- to be equivalent to a MonadDelegate that doesn't fire anything.
-- -- Outer MaybeT is for Alternative instance.
-- -- DelegateT ensures that it is impossible to create an ContT that
-- -- doesn't call its continuation, instead it will result in empty.
-- newtype DelegateT m a = DelegateT { unDelegateT :: MaybeT (ContT () (MaybeT m)) a }
--     -- deriving (Functor, ) via (MaybeT (ContT () (MaybeT m)))
--     deriving ( Functor
--         , Applicative
--         , Alternative
--         , Monad
--         , MonadIO
--         , MonadCont
--         )

-- deriving via (ContT () (MaybeT m)) instance Monad m => MonadDelegate (AContT () (MaybeT m))
-- deriving via (MaybeT (ContT () (MaybeT m))) instance Monad m => MonadDelegate (DelegateT m)

-- deriving via (MaybeT (ContT () (MaybeT m))) instance Monad m => MonadIO (DelegateT m)

-- -- | Instance that does real work using continuations
-- instance Monad m => MonadDelegate (DelegateT m) where
--     -- | after delegating, verify if the fire function has been called
--     -- protect against the fire not being called (and thus resulting in a
--     -- a ContT that can't be bind)
--     -- k :: (Maybe a -> MaybeT m ())
--     delegate f = ContT $ \k -> evalContT $ f (lift . k)
--       where
--         -- f :: (a -> DelegateT m ()) -> DelegateT m ()
--         -- f' :: (a -> MaybeT (StateT s (ContT () (MaybeT m))) -> MaybeT (StateT s (ContT () (MaybeT m))
--         f'

-- -- | Instance that does real work using continuations
-- instance Monad m => MonadTerminate (DelegateT m) where
--     terminate = DelegateT $ lift $ ContT $ \_ -> empty
--     -- terminate = empty
--     -- terminally (DelegateT $ MaybeT m) = DelegateT $ delegate $ \fire ->m
--     -- delegate f = DelegateT $ MaybeT $ ContT $ \k -> evalContT $ f (lift . k)


