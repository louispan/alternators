module Control.Monad.Trans.Reader.Extras where

import Control.Monad.Trans.Reader
import Control.Monad.Morph
import Data.Functor.Identity

-- | This function combines two different monadic effects,
-- where one of the effects is a reader effect and the other is a
-- producing effect.
--
-- This version results in the reader monad's inner effect to be wrapped
-- around the producing effect.
-- This requires the Reader inner effect to be an MFunctor on Identity.
--
-- This can enable past-Dependence.
-- Elm has foldp : (a -> state -> state) -> state -> Signal a -> Signal state
-- This is equivalent to a creating a @StateT state (Signal m) ()@
--
-- 'runReaderM' is a more general form of @StateT state (Signal m) ()@ where
-- given a reader monad to transform "a" to "c" with effects, and an "as"
-- monad that produces "a"s with other effects, run the result of "as" through
-- the reader monad to produce "c"s with both effects.
-- @
-- runReaderM :: Monad m => Reader a (State s) c -> m a                      -> StateT s m c
-- runReaderM ::            Reader a (State s) c -> Signal STM a             -> StateT state (Signal STM) c
-- runReaderM ::            Reader a (State s) c -> Pipes.Concurrent.Input a -> StateT state Pipes.Concurrent.Input c
-- @
runReaderM :: (Monad m, Monad (t m), MonadTrans t, MFunctor t)
  => ReaderT a (t Identity) c -> m a -> t m c
runReaderM c as = do
  a <- lift as
  hoist generalize $ runReaderT c a

-- | An alternate form of runReaderM where the producing effect is
-- wrapped around the reader monad's inner effect.
-- This requires the producing effect to be an MFunctor on Identity.
runReaderM' :: (Monad m, Monad (t m), MonadTrans t, MFunctor t)
  => ReaderT a m c -> (t Identity) a -> t m c
runReaderM' c as = do
  a <- hoist generalize as
  lift $ runReaderT c a
