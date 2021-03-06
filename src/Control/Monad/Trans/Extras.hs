{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Monad.Trans.Extras where

import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Data.Maybe

-- | It is useful to use 'evalMaybeT' flipped so it can be in a chain of
-- transformer runners, like this @evalCont . (`evalMaybeT` val)@
-- This argument ordering is consistent with 'Control.Monad.Trans.State.evalStateT'.
evalMaybeT :: Functor m => MaybeT m a -> a -> m a
evalMaybeT m a = (fromMaybe a) <$> (runMaybeT m)

-- -- | Use 'Just' froma Maybe' value using 'Alternative'
guardJust :: (Monad m, Alternative m) => Maybe a -> m a
guardJust = maybe empty pure

-- | mtl-like use of 'MaybeT' for any transformer stack that is an instance
-- of 'Alternative'. Can be used instead of 'MaybeT'.
guardJustM :: (Monad m, Alternative m) => m (Maybe a) -> m a
guardJustM = (>>= guardJust)

-- | Use this type synonym whenever you want both 'MonadIO' and 'Alternative' whilst
-- ensuring concrete transformer stacks use a MaybeT and not the IO for the 'Alternative'.
type AlternativeIO m = (MonadIO m, SafeAlternative m)

-- | Safely combined 'liftIO' and 'guardJustM', by adding the 'AlternativeIO' constraint
guardJustIO :: AlternativeIO m => IO (Maybe a) -> m a
guardJustIO = guardJustM . liftIO

-- | The IO instance of Applicative and MonadPlus is dangerous as it it too easy
-- to accidentally omit a MaybeT in the transformer stack and introduce exceptions.
-- Make a pull request to add to this list.
type family (IsSafeAlternative a) :: Bool where
  IsSafeAlternative IO = 'False
  IsSafeAlternative a = 'True

-- This class ensures that there is a MaybeT in a transformer stack.
class Alternative m => SafeAlternative (m :: * -> *)

-- | Any transformer on top of 'SafeAlternative' is also a 'SafeAlternative'
instance {-# OVERLAPPABLE #-} (Alternative (t m), MonadTrans t, SafeAlternative m) => SafeAlternative (t m)

-- | The real instance that fulfills the the 'SafeAlternative' constraint
-- This instance terminates the above transformer instance search.
instance {-# OVERLAPPABLE #-} Monad m => SafeAlternative (MaybeT m)

-- | Anything except IO is a safe alternative instance
instance {-# OVERLAPPABLE #-} (IsSafeAlternative m ~ 'True, Alternative m) => SafeAlternative m

-- -- | Adds the 'SafeAlternative' constraint
-- safeAlternatively :: SafeAlternative m => m a -> m a
-- safeAlternatively = id

