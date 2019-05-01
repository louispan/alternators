{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Monad.Env where

import Control.Monad.Reader
import Control.Monad.Morph

-- | A copy of 'MonadReader' with overlapping instances
class Monad m => MonadEnv r m where
    askEnv :: m r
    -- localEnv :: (r -> r) -> m a -> m a

instance {-# OVERLAPPABLE #-} (Monad (t m), MonadTrans t, MonadEnv r m) => MonadEnv r (t m) where
    askEnv = lift askEnv
    -- localEnv f m = hoist (localEnv f) m

instance {-# OVERLAPPABLE #-} Monad m => MonadEnv r (ReaderT r m) where
    askEnv = ask
    -- localEnv = local

-- | A copy of 'MonadReader' with overlapping instances
class Monad m => MonadEnv' f r m | m -> r where
    askEnv' :: m (f r)

instance {-# OVERLAPPABLE #-} (Monad (t m), MonadTrans t, MonadEnv' f r m) => MonadEnv' f r (t m) where
    askEnv' = lift askEnv'

instance {-# OVERLAPPABLE #-} Monad m => MonadEnv' f r (ReaderT (f r) m) where
    askEnv' = ask
