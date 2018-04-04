module Control.Monad.Trans.TMCont where

import Control.Concurrent.STM
import Control.Monad.STM.Class
import Control.Monad.Trans
import Control.Monad.Trans.MCont

-- | Given an action continuation (eg 'write' a command), thread a TMVar around it.
-- This allows for parallel interpretation when multiples tmContT is combined
-- applicatively.
-- When combined monadically, only one command is written, so the interpretation
-- is sequential.
tmContT :: MonadSTM m => ((m a -> m ()) -> m ()) -> MContT r m a
tmContT m = do
    v <- liftSTM newEmptyTMVar
    lift $ m (\ma -> ma >>= liftSTM . putTMVar v)
    MContT $ \k ->
            (k (liftSTM $ takeTMVar v))

-- | Variation of 'tmContT' that accepts a vanilla value type continuation.
tmContT' :: MonadSTM m => ((a -> m ()) -> m ()) -> MContT r m a
tmContT' m = tmContT (\k -> m (\a -> k (pure a)))

tmContT :: ((m a -> m ()) -> m ()) -> MContT r m a
tmContT m = do
    v <- liftSTM newEmptyTMVar
    lift $ m (\ma -> ma >>= liftSTM . putTMVar v)
    MContT $ \k ->
            (k (liftSTM $ takeTMVar v))
