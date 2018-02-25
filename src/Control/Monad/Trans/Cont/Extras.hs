{-# LANGUAGE ExplicitForAll #-}

module Control.Monad.Trans.Cont.Extras where

import Control.Monad.Trans.Cont

seqContT :: Applicative m => ContT () m b -> ContT () m b -> ContT () m b
seqContT (ContT f) (ContT g) = ContT $ \k -> f k *> g k
infixl 4 `seqContT` -- like *>

-- liftedMappendCont :: (Applicative m, Semigroup r) => ContT r m b -> ContT r m b -> ContT r m b
-- liftedMappendCont (ContT f) (ContT g) = ContT $ \k -> liftA2 (<>) (f k) (g k)
-- infixl 4 `liftedMappendCont` -- like <>

-- | Convert the original ContT to a ContT that
-- doens't call it's continuation, by 'const'ing the original contination
-- to 'pure'.
terminate :: forall b m. Applicative m => ContT () m () -> ContT () m b
terminate = withContT (const $ pure)
