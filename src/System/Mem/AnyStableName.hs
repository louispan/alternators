{-# LANGUAGE ExistentialQuantification #-}

module System.Mem.AnyStableName where

import Data.Hashable
import System.Mem.StableName

-- | Erases the type variable @a@ in 'StableName'
data AnyStableName = forall a. AnyStableName (StableName a)

makeAnyStableName :: a -> IO AnyStableName
makeAnyStableName = fmap AnyStableName . makeStableName

instance Eq AnyStableName where
    AnyStableName x == AnyStableName y = eqStableName x y

instance Hashable AnyStableName where
    hashWithSalt s (AnyStableName x) = s `hashWithSalt` (hashStableName x)
    hash (AnyStableName x) = hashStableName x
