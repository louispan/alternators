{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}

module Data.Tagged.Extras where

import Data.Tagged

-- | 'retag' where you can specify @s@ and @t@ via TypeApplications
retag' :: forall s t a. Tagged s a -> Tagged t a
retag' = retag