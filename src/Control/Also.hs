{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Also where

import Control.Applicative
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
import Data.Functor.Identity

-- | Combining effects where both input effects are used as much as possible.
-- as opposed to 'Control.Applicative.Alternative' where only the "successful" effect is used.
class Also m a where
    -- | An associative binary operation, where both input effects are used as much as possible.
    also :: m a -> m a -> m a
    -- | The identity of 'also'
    alsoZero :: m a

infixr 6 `also` -- like <>

instance {-# OVERLAPPABLE #-} (Monoid a, Applicative f) => Also f a where
    alsoZero = pure mempty
    f `also` g = liftA2 (<>) f g

instance (Monoid a) => Also Identity a where
    alsoZero = mempty
    a `also` b = a <> b

instance (Monoid a) => Also IO a where
    alsoZero = mempty
    a `also` b = a <> b

instance (Also m a) => Also (IdentityT m) a where
    alsoZero = IdentityT alsoZero
    (IdentityT a) `also` (IdentityT b) = IdentityT $ a `also` b

-- | Combine the monads that returns @r@ not @a@.
instance (Also m r) => Also (ContT r m) a where
    alsoZero = ContT . const $ alsoZero
    (ContT f) `also` (ContT g) =
        ContT $ \k -> (f k) `also` (g k)

instance (Also m a) => Also (ReaderT r m) a where
    alsoZero = ReaderT $ const alsoZero
    (ReaderT f) `also` (ReaderT g) = ReaderT $ \r -> f r `also` g r

instance (Also m (Either e a)) => Also (ExceptT e m) a where
    alsoZero = ExceptT $ alsoZero
    (ExceptT f) `also` (ExceptT g) = ExceptT $ f `also` g

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
