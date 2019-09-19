{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}

module Data.Tagged.Extras
( module Data.Tagged
, retag'
, untag'
) where

import Data.Hashable
import Data.Tagged

-- | 'retag' where you can specify @s@ and @t@ via TypeApplications
retag' :: forall s t a. Tagged s a -> Tagged t a
retag' = retag

-- | 'untag' where you can specify @s@ and @t@ via TypeApplications
untag' :: forall s a. Tagged s a -> a
untag' = untag

instance Hashable a => Hashable (Tagged s a) where
    hashWithSalt s (Tagged x) = s `hashWithSalt` x
    hash (Tagged x) = hash x
