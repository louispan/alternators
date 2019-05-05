{-# LANGUAGE DerivingVia #-}

module Control.Monad.Trans.Extras where

import Control.Applicative
import Control.Monad.Trans.Maybe
import Data.Maybe

-- | It is useful to use 'evalMaybeT' flipped so it can be in a chain of
-- transformer runners, like this @evalCont . (`evalMaybeT` val)@
-- This argument ordering is consistent with 'Control.Monad.Trans.State.evalStateT'.
evalMaybeT :: Functor m => MaybeT m a -> a -> m a
evalMaybeT m a = (fromMaybe a) <$> (runMaybeT m)

-- | mtl-like use of MaybeT
whenMaybe :: Alternative f => Maybe a -> f a
whenMaybe = maybe empty pure