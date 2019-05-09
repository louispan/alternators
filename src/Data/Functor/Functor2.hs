module Data.Functor.Functor2 where

-- | 'Functor' for binary type constructors
class Functor2 t where
    fmap2 :: (m a -> n b) -> t m a -> t n b
