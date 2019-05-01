{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Monad.Observer where

import Control.Monad.Reader

class Monad m => MonadObserver a m | m -> a where
    askObserver :: m (a -> m ())

instance {-# OVERLAPPABLE #-} (Monad (t m), MonadTrans t, MonadObserver a m) => MonadObserver a (t m) where
    askObserver = lift $ (lift .) <$> askObserver

instance {-# OVERLAPPABLE #-} Monad m => MonadObserver a (ReaderT (a -> m ()) m) where
    askObserver = (lift .) <$> ask

observe :: MonadObserver a m => a -> m ()
observe a = askObserver >>= ($ a)

