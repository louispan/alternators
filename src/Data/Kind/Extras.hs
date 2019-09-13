{-# LANGUAGE TypeFamilies #-}

module Data.Kind.Extras where

import Data.Functor.Identity

-- | "Higher-Kinded Data"
-- Erases 'Identity'
-- from https://reasonablypolymorphic.com/blog/higher-kinded-data/
type family HKD f a where
  HKD Identity a = a
  HKD f        a = f a