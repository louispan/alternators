module Data.Function.Extras where

-- | 'undefined' with a compile warning to remove usage.
fixme :: a
fixme = undefined
{-# WARNING fixme "FIXME" #-}

-- | 'id' with a compile warning to remove usage.
hack :: a -> a
hack = id
{-# WARNING hack "HACK" #-}

-- | Useful for double-checking discard values when using '*>', etc
voided :: m () -> m ()
voided = id