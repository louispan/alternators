module Control.Monad.Trans.Maybe.Extras where

import Control.Applicative
import Control.Monad.Trans.Maybe

fromMaybeT :: (Alternative t, Functor m) => MaybeT m a -> m (t a)
fromMaybeT = fmap (maybe empty pure) . runMaybeT
