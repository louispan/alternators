module Data.Function.Extras where

-- | 'undefined' with a compile warning to remove usage.
hack :: a
hack = undefined
{-# WARNING hack "HACK" #-}

-- | 'id' with a compile warning to remove usage.
fixme :: a -> a
fixme = id
{-# WARNING fixme "FIXME" #-}

-- | Useful for double-checking discard values when using '*>', etc
voided :: m () -> m ()
voided = id
