module Data.Functor.Extras where

-- | Useful for double-checking discard values when using '*>', etc
voided :: m () -> m ()
voided = id