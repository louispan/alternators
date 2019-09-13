{-# LANGUAGE ExistentialQuantification #-}

module Data.AnyStableName where

import Data.Hashable
import System.Mem.StableName

data AnyStableName = forall a. AnyStableName (StableName a)

instance Eq AnyStableName where
    AnyStableName x == AnyStableName y = eqStableName x y

instance Hashable AnyStableName where
    hashWithSalt s (AnyStableName x) = s `hashWithSalt` (hashStableName x)
    hash (AnyStableName x) = hashStableName x
