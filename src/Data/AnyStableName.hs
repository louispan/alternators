-- {-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE ExistentialQuantification #-}

module Data.AnyStableName where

import Data.Hashable
import System.Mem.StableName

data AnyStableName = forall a. AnyStableName (StableName a)

instance Eq AnyStableName where
    AnyStableName x == AnyStableName y = eqStableName x y

instance Hashable AnyStableName where
    hashWithSalt s x = s `hashWithSalt` (hashAnyStableName x)
    hash = hashAnyStableName

hashAnyStableName :: AnyStableName -> Int
hashAnyStableName (AnyStableName x) = hashStableName x