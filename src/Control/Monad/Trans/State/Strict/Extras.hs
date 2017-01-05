module Control.Monad.Trans.State.Strict.Extras where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State.Strict

maybeState :: Monad m => StateT s (MaybeT m) a -> MaybeT (StateT s m) a
maybeState sm = MaybeT $ do
    s <- get
    r <- lift $ runMaybeT $ runStateT sm s
    case r of
        Nothing -> pure Nothing
        Just (a, s') -> do
            put s'
            pure (Just a)

exceptState :: Monad m => StateT s (ExceptT e m) a -> ExceptT e (StateT s m) a
exceptState sm = ExceptT $ do
    s <- get
    r <- lift $ runExceptT $ runStateT sm s
    case r of
        Left e -> pure $ Left e
        Right (a, s') -> do
            put s'
            pure (Right a)
