{-# LANGUAGE CPP #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Also where

import Control.Applicative
import Data.Proxy
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Cont
import Control.Monad.Trans.Except
import Control.Monad.Trans.Identity
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
import Control.Monad.Trans.RWS.Lazy as Lazy
import Control.Monad.Trans.RWS.Strict as Strict
import Control.Monad.Trans.State.Lazy as Lazy
import Control.Monad.Trans.State.Strict as Strict
import Control.Monad.Trans.Writer.Lazy as Lazy
import Control.Monad.Trans.Writer.Strict as Strict
import Control.Newtype.Generics
import Data.Functor.Identity
import GHC.Generics

#if MIN_VERSION_base(4,12,0)
import Data.Monoid (Ap)
#endif

#if MIN_VERSION_base(4,9,0) && !MIN_VERSION_base(4,10,0)
import Data.Semigroup
#endif

-- | Combining effects where both input effects are used as much as possible.
-- as opposed to 'Control.Applicative.Alternative' where only the "successful" effect is used.
class Also f a where
    -- | An associative binary operation, where both input effects are used as much as possible.
    also :: f a -> f a -> f a
    -- | The identity of 'also'
    alsoZero :: f a

infixr 6 `also` -- like <>

-- | Monoid under 'also'.
-- Ie. Allow 'also' for 'Monoid' effects
-- Mnemonic: 'Als' for 'Also', just like 'Alt' for 'Altenative'
newtype Als f a = Als { getAls :: f a }
    deriving (Generic, Generic1, Read, Show, Eq, Ord, Num, Enum,
                Monad, MonadPlus, Applicative, Alternative, Functor)

instance Newtype (Als f a)

instance Also f a => Semigroup (Als f a) where
    (Als f) <> (Als g) = Als (f `also` g)

instance Also f a => Monoid (Als f a) where
    mempty = Als alsoZero
#if !MIN_VERSION_base(4,11,0)
    (Als f) `mappend` (Als g) = Als (f `also` g)
#endif

#if MIN_VERSION_base(4,12,0)
-- | 'also' under '<|>'.
-- Ie. Allow use '<>' for 'Also' effects
-- The 'Also' equivalent of 'Ap'
instance (Monoid a, Applicative f) => Also (Ap f) a where
    (Ap f) `also` (Ap g) = f <> g
    alsoZero = mempty
#endif

-- | Overlappable instance for all Monoids.
-- instance {-# OVERLAPPABLE #-} (Monoid (f a)) => Also f a where
--     alsoZero = mempty
--     also = mappend

-- -- overlapping instance to passthrough Also to return type
-- instance Also m a => Also IO (m a) where
--     alsoZero = pure alsoZero -- or should it be pure ()?
--     f `also` g = liftA2 also f g

-- instance  Semigroup a => Also Maybe a where
--     alsoZero = Nothing
--     Nothing `also` r = r
--     r `also` Nothing = r
--     (Just x) `also` (Just y) = Just (x <> y)

-- terminating instance
instance Also IO () where
    alsoZero = pure ()
    also = (*>)

-- terminating instance
instance Also Identity () where
    alsoZero = pure ()
    also = (*>)

-- same instance as ReaderT
instance (Also m a) => Also ((->) r) (m a) where
    alsoZero = const alsoZero
    f `also` g = \r -> f r `also` g r

-- | passthrough instance
instance (Also m a) => Also (ReaderT r m) a where
    alsoZero = ReaderT $ const alsoZero
    (ReaderT f) `also` (ReaderT g) = ReaderT $ \r -> f r `also` g r

-- | passthrough instance
instance (Also m a) => Also (IdentityT m) a where
    alsoZero = IdentityT alsoZero
    (IdentityT a) `also` (IdentityT b) = IdentityT $ a `also` b

-- | Note: this instance combines monads that returns @r@ not @a@.
instance (Also m r) => Also (ContT r m) a where
    alsoZero = ContT . const $ alsoZero
    (ContT f) `also` (ContT g) =
        ContT $ \k -> (f k) `also` (g k)

-- | passthrough instance, but only if the inner monad
-- is a 'Also' for m (Either e a)'
-- Usually this means a ContT in the inner monad stack
instance (Also m (Either e a)) => Also (ExceptT e m) a where
    alsoZero = ExceptT $ alsoZero
    (ExceptT f) `also` (ExceptT g) = ExceptT $ f `also` g

-- | passthrough instance, but only if the inner monad
-- is a 'Also' for m (Either e a)'
-- Usually this means a ContT in the inner monad stack
instance (Also m (Maybe a)) => Also (MaybeT m) a where
    alsoZero = MaybeT $ alsoZero
    (MaybeT f) `also` (MaybeT g) = MaybeT $ f `also` g

-- | State instances threads the state through both monad of 'also'
-- in the normal left to right order, and so do not prevent
-- early termination from the left monad (eg if the inner monad was
-- a 'MaybeT' or 'ExceptT'.
-- However, it is able to use the 'also' to combine the return value.
instance (Also m a, Monad m) => Also (Lazy.StateT s m) a where
    alsoZero = lift alsoZero
    f `also` g = do
        (x, y) <- liftA2 (,) f g
        lift $ pure x `also` pure y

-- | State instances threads the state through both monad of 'also'
-- in the normal left to right order, and so do not prevent
-- early termination from the left monad (eg if the inner monad was
-- a 'MaybeT' or 'ExceptT'.
-- However, it is able to use the 'also' to combine the return value.
instance (Also m a, Monad m) => Also (Strict.StateT s m) a where
    alsoZero = lift alsoZero
    f `also` g = do
        (x, y) <- liftA2 (,) f g
        lift $ pure x `also` pure y

-- | Writer instances threads the writer through both monad of 'also'
-- in the normal left to right order, and so do not prevent
-- early termination from the left monad (eg if the inner monad was
-- a 'MaybeT' or 'ExceptT'.
-- However, it is able to use the 'also' to combine the return value.
instance (Monoid w, Also m a, Monad m) => Also (Lazy.WriterT w m) a where
    alsoZero = lift alsoZero
    f `also` g = do
        (x, y) <- liftA2 (,) f g
        lift $ pure x `also` pure y

-- | Writer instances threads the writer through both monad of 'also'
-- in the normal left to right order, and so do not prevent
-- early termination from the left monad (eg if the inner monad was
-- a 'MaybeT' or 'ExceptT'.
-- However, it is able to use the 'also' to combine the return value.
instance (Monoid w, Also m a, Monad m) => Also (Strict.WriterT w m) a where
    alsoZero = lift alsoZero
    f `also` g = do
        (x, y) <- liftA2 (,) f g
        lift $ pure x `also` pure y

-- | State instances threads the state through both monad of 'also'
-- in the normal left to right order, and so do not prevent
-- early termination from the left monad (eg if the inner monad was
-- a 'MaybeT' or 'ExceptT'.
-- However, it is able to use the 'also' to combine the return value.
instance (Monoid w, Also m a, Monad m) => Also (Lazy.RWST r w s m) a where
    alsoZero = lift alsoZero
    f `also` g = do
        (x, y) <- liftA2 (,) f g
        lift $ pure x `also` pure y

-- | State instances threads the state through both monad of 'also'
-- in the normal left to right order, and so do not prevent
-- early termination from the left monad (eg if the inner monad was
-- a 'MaybeT' or 'ExceptT'.
-- However, it is able to use the 'also' to combine the return value.
instance (Monoid w, Also m a, Monad m) => Also (Strict.RWST r w s m) a where
    alsoZero = lift alsoZero
    f `also` g = do
        (x, y) <- liftA2 (,) f g
        lift $ pure x `also` pure y

