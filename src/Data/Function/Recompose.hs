{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Function.Recompose where

-- | Compose in a way that function arguments are deferred to the resultant function.
-- Eg, it is like the dot '.' composition operator,
-- but the left function doesn't have to be a unary function
-- but can also be an n-ary function.
-- Eg, it is like 'fmap' for (a ->) functor,
-- but also for (a -> b ->), (a -> b -> c ->), ..
-- f `recompose` g x -> f (g x)
-- f `recompose` g a x -> f (g a x)
-- f `recompose` g a b x -> f (g a b x)
class Recompose f g x | f g -> x where
    recompose :: f -> g -> x

instance {-# OVERLAPPABLE #-} Recompose (x -> y) (a -> x) (a -> y) where
    recompose = (.)

instance {-# OVERLAPPABLE #-} Recompose (x -> y) (a -> b -> x) (a -> b -> y) where
    recompose f' g' a b = f' (g' a b)

instance {-# OVERLAPPABLE #-} Recompose (x -> y) (a -> b -> c -> x) (a -> b -> c -> y) where
    recompose f' g' a b c = f' (g' a b c)

instance {-# OVERLAPPABLE #-} Recompose (x -> y) (a -> b -> c -> d -> x) (a -> b -> c -> d -> y) where
    recompose f' g' a b c d = f' (g' a b c d)

instance {-# OVERLAPPABLE #-} Recompose (x -> y) (a -> b -> c -> d -> e -> x) (a -> b -> c -> d -> e -> y) where
    recompose f' g' a b c d e = f' (g' a b c d e)

instance {-# OVERLAPPABLE #-} Recompose (x -> y) (a -> b -> c -> d -> e -> f -> x) (a -> b -> c -> d -> e -> f -> y) where
    recompose f' g' a b c d e f = f' (g' a b c d e f)

instance {-# OVERLAPPABLE #-} Recompose (x -> y) (a -> b -> c -> d -> e -> f -> g -> x) (a -> b -> c -> d -> e -> f -> g -> y) where
    recompose f' g' a b c d e f g = f' (g' a b c d e f g)

instance {-# OVERLAPPABLE #-} Recompose (x -> y) (a -> b -> c -> d -> e -> f -> g -> h -> x) (a -> b -> c -> d -> e -> f -> g -> h -> y) where
    recompose f' g' a b c d e f g h = f' (g' a b c d e f g h)

instance {-# OVERLAPPABLE #-} Recompose (x -> y) (a -> b -> c -> d -> e -> f -> g -> h -> i -> x) (a -> b -> c -> d -> e -> f -> g -> h -> i -> y) where
    recompose f' g' a b c d e f g h i = f' (g' a b c d e f g h i)

instance {-# OVERLAPPABLE #-} Recompose (x -> y) (a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> x) (a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> y) where
    recompose f' g' a b c d e f g h i j = f' (g' a b c d e f g h i j)

instance {-# OVERLAPPABLE #-} Recompose (x -> y) (a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> k -> x) (a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> k -> y) where
    recompose f' g' a b c d e f g h i j k = f' (g' a b c d e f g h i j k)

instance {-# OVERLAPPABLE #-} Recompose (x -> y) (a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> k -> l -> x) (a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> k -> l -> y) where
    recompose f' g' a b c d e f g h i j k l = f' (g' a b c d e f g h i j k l)

instance {-# OVERLAPPABLE #-} Recompose (x -> y) (a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> k -> l -> m -> x) (a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> k -> l -> m -> y) where
    recompose f' g' a b c d e f g h i j k l m = f' (g' a b c d e f g h i j k l m)

instance {-# OVERLAPPABLE #-} Recompose (x -> y) (a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> k -> l -> m -> n -> x) (a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> k -> l -> m -> n -> y) where
    recompose f' g' a b c d e f g h i j k l m n = f' (g' a b c d e f g h i j k l m n)

instance {-# OVERLAPPABLE #-} Recompose (x -> y) (a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> k -> l -> m -> n -> o -> x) (a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> k -> l -> m -> n -> o -> y) where
    recompose f' g' a b c d e f g h i j k l m n o = f' (g' a b c d e f g h i j k l m n o)

(.*) :: Recompose f g x => f -> g -> x
(.*) = recompose

infixr 9 .*
infixr 9 `recompose`