{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module Data.UpCast where

import Control.Newtype.Generics

class UpCast a where
    type Parent
    upcast :: a -> CastParent



upcastNewtype :: Newtype a => a -> O a
upcastNewtype = unpack
