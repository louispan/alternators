{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}

-- | Binds the n-th argument of a m-numery function (that retuns in a monad)
-- with the result of a given monad.
-- This results in a a (m-1) numery function that doesn't require the bound argument.
-- The conversion is a pure calculation, not under the monad.
-- Example (more examples in the test/Spec.hs):
-- @
-- Given a function that takse 3 args and returns a monad
-- f :: a -> b -> c -> m d
--
-- and a monad that provides the 2nd arg
-- mb :: m b

-- If you want to "simply" the "f" to a function that only need 2 args
-- with the previous 2nd arg bound to the result of "mb"
-- g :: a -> c -> m d

-- Specifying my hand
-- g a c = do
--     b <- mb
--     f a b c
--
-- Using Control.Monad.Bind
-- g = f `bind2` mb
-- @
module Control.Monad.Bind where

import Data.Kind

type family Result rest :: Type where
    Result (a -> b) = Result b
    Result a = a

type family UnaryType r :: Type -> Type where
    UnaryType (m a) = m

class Monad (UnaryType (Result rest)) => Bind1 rest where
    bind1 :: (a -> rest) -> (UnaryType (Result rest)) a -> rest

instance {-# OVERLAPPABLE #-} (Monad m, UnaryType (Result (m x)) ~ m) => Bind1 (m x) where
    bind1 f' ma = ma >>= f'

instance {-# OVERLAPPABLE #-} (Monad m, UnaryType (Result (a -> m x)) ~ m) => Bind1 (a -> m x) where
    bind1 f' my a = do
        y <- my
        f' y a

instance {-# OVERLAPPABLE #-} (Monad m, UnaryType (Result (a -> b -> m x)) ~ m) => Bind1 (a -> b -> m x) where
    bind1 f' my a b = do
        y <- my
        f' y a b

instance {-# OVERLAPPABLE #-} (Monad m, UnaryType (Result (a -> b -> c -> m x)) ~ m) => Bind1 (a -> b -> c -> m x) where
    bind1 f' my a b c = do
        y <- my
        f' y a b c

instance {-# OVERLAPPABLE #-} (Monad m, UnaryType (Result (a -> b -> c -> d -> m x)) ~ m) => Bind1 (a -> b -> c -> d -> m x) where
    bind1 f' my a b c d = do
        y <- my
        f' y a b c d

instance {-# OVERLAPPABLE #-} (Monad m, UnaryType (Result (a -> b -> c -> d -> e -> m x)) ~ m) => Bind1 (a -> b -> c -> d -> e -> m x) where
    bind1 f' my a b c d e = do
        y <- my
        f' y a b c d e

instance {-# OVERLAPPABLE #-} (Monad m, UnaryType (Result (a -> b -> c -> d -> e -> f -> m x)) ~ m) => Bind1 (a -> b -> c -> d -> e -> f -> m x) where
    bind1 f' my a b c d e f = do
        y <- my
        f' y a b c d e f

instance {-# OVERLAPPABLE #-} (Monad m, UnaryType (Result (a -> b -> c -> d -> e -> f -> g -> m x)) ~ m) => Bind1 (a -> b -> c -> d -> e -> f -> g -> m x) where
    bind1 f' my a b c d e f g = do
        y <- my
        f' y a b c d e f g

instance {-# OVERLAPPABLE #-} (Monad m, UnaryType (Result (a -> b -> c -> d -> e -> f -> g -> h -> m x)) ~ m) => Bind1 (a -> b -> c -> d -> e -> f -> g -> h -> m x) where
    bind1 f' my a b c d e f g h = do
        y <- my
        f' y a b c d e f g h

instance {-# OVERLAPPABLE #-} (Monad m, UnaryType (Result (a -> b -> c -> d -> e -> f -> g -> h -> i -> m x)) ~ m) => Bind1 (a -> b -> c -> d -> e -> f -> g -> h -> i -> m x) where
    bind1 f' my a b c d e f g h i = do
        y <- my
        f' y a b c d e f g h i

class Monad (UnaryType (Result rest)) => Bind2 rest where
    bind2 :: (a -> b -> rest) -> (UnaryType (Result rest)) b -> a -> rest

instance {-# OVERLAPPABLE #-} (Monad m, UnaryType (Result (m x)) ~ m) => Bind2 (m x) where
    bind2 f' my a = do
        y <- my
        f' a y

instance {-# OVERLAPPABLE #-} (Monad m, UnaryType (Result (a -> m x)) ~ m) => Bind2 (a -> m x) where
    bind2 f' my a b = do
        y <- my
        f' a y b

instance {-# OVERLAPPABLE #-} (Monad m, UnaryType (Result (a -> b -> m x)) ~ m) => Bind2 (a -> b -> m x) where
    bind2 f' my a b c = do
        y <- my
        f' a y b c

instance {-# OVERLAPPABLE #-} (Monad m, UnaryType (Result (a -> b -> c -> m x)) ~ m) => Bind2 (a -> b -> c -> m x) where
    bind2 f' my a b c d = do
        y <- my
        f' a y b c d

instance {-# OVERLAPPABLE #-} (Monad m, UnaryType (Result (a -> b -> c -> d -> m x)) ~ m) => Bind2 (a -> b -> c -> d -> m x) where
    bind2 f' my a b c d e = do
        y <- my
        f' a y b c d e

instance {-# OVERLAPPABLE #-} (Monad m, UnaryType (Result (a -> b -> c -> d -> e -> m x)) ~ m) => Bind2 (a -> b -> c -> d -> e -> m x) where
    bind2 f' my a b c d e f = do
        y <- my
        f' a y b c d e f

instance {-# OVERLAPPABLE #-} (Monad m, UnaryType (Result (a -> b -> c -> d -> e -> f -> m x)) ~ m) => Bind2 (a -> b -> c -> d -> e -> f -> m x) where
    bind2 f' my a b c d e f g = do
        y <- my
        f' a y b c d e f g

instance {-# OVERLAPPABLE #-} (Monad m, UnaryType (Result (a -> b -> c -> d -> e -> f -> g -> m x)) ~ m) => Bind2 (a -> b -> c -> d -> e -> f -> g -> m x) where
    bind2 f' my a b c d e f g h = do
        y <- my
        f' a y b c d e f g h

instance {-# OVERLAPPABLE #-} (Monad m, UnaryType (Result (a -> b -> c -> d -> e -> f -> g -> h -> m x)) ~ m) => Bind2 (a -> b -> c -> d -> e -> f -> g -> h -> m x) where
    bind2 f' my a b c d e f g h i = do
        y <- my
        f' a y b c d e f g h i

class Monad (UnaryType (Result rest)) => Bind3 rest where
    bind3 :: (a -> b -> c -> rest) -> (UnaryType (Result rest)) c -> a -> b -> rest

instance {-# OVERLAPPABLE #-} (Monad m, UnaryType (Result (m x)) ~ m) => Bind3 (m x) where
    bind3 f' my a b = do
        y <- my
        f' a b y

instance {-# OVERLAPPABLE #-} (Monad m, UnaryType (Result (a -> m x)) ~ m) => Bind3 (a -> m x) where
    bind3 f' my a b c = do
        y <- my
        f' a b y c

instance {-# OVERLAPPABLE #-} (Monad m, UnaryType (Result (a -> b -> m x)) ~ m) => Bind3 (a -> b -> m x) where
    bind3 f' my a b c d = do
        y <- my
        f' a b y c d

instance {-# OVERLAPPABLE #-} (Monad m, UnaryType (Result (a -> b -> c -> m x)) ~ m) => Bind3 (a -> b -> c -> m x) where
    bind3 f' my a b c d e = do
        y <- my
        f' a b y c d e

instance {-# OVERLAPPABLE #-} (Monad m, UnaryType (Result (a -> b -> c -> d -> m x)) ~ m) => Bind3 (a -> b -> c -> d -> m x) where
    bind3 f' my a b c d e f = do
        y <- my
        f' a b y c d e f

instance {-# OVERLAPPABLE #-} (Monad m, UnaryType (Result (a -> b -> c -> d -> e -> m x)) ~ m) => Bind3 (a -> b -> c -> d -> e -> m x) where
    bind3 f' my a b c d e f g = do
        y <- my
        f' a b y c d e f g

instance {-# OVERLAPPABLE #-} (Monad m, UnaryType (Result (a -> b -> c -> d -> e -> f -> m x)) ~ m) => Bind3 (a -> b -> c -> d -> e -> f -> m x) where
    bind3 f' my a b c d e f g h = do
        y <- my
        f' a b y c d e f g h

instance {-# OVERLAPPABLE #-} (Monad m, UnaryType (Result (a -> b -> c -> d -> e -> f -> g -> m x)) ~ m) => Bind3 (a -> b -> c -> d -> e -> f -> g -> m x) where
    bind3 f' my a b c d e f g h i = do
        y <- my
        f' a b y c d e f g h i

class Monad (UnaryType (Result rest)) => Bind4 rest where
    bind4 :: (a -> b -> c -> d -> rest) -> (UnaryType (Result rest)) d -> a -> b -> c -> rest

instance {-# OVERLAPPABLE #-} (Monad m, UnaryType (Result (m x)) ~ m) => Bind4 (m x) where
    bind4 f' my a b c = do
        y <- my
        f' a b c y

instance {-# OVERLAPPABLE #-} (Monad m, UnaryType (Result (a -> m x)) ~ m) => Bind4 (a -> m x) where
    bind4 f' my a b c d = do
        y <- my
        f' a b c y d

instance {-# OVERLAPPABLE #-} (Monad m, UnaryType (Result (a -> b -> m x)) ~ m) => Bind4 (a -> b -> m x) where
    bind4 f' my a b c d e = do
        y <- my
        f' a b c y d e

instance {-# OVERLAPPABLE #-} (Monad m, UnaryType (Result (a -> b -> c -> m x)) ~ m) => Bind4 (a -> b -> c -> m x) where
    bind4 f' my a b c d e f = do
        y <- my
        f' a b c y d e f

instance {-# OVERLAPPABLE #-} (Monad m, UnaryType (Result (a -> b -> c -> d -> m x)) ~ m) => Bind4 (a -> b -> c -> d -> m x) where
    bind4 f' my a b c d e f g = do
        y <- my
        f' a b c y d e f g

instance {-# OVERLAPPABLE #-} (Monad m, UnaryType (Result (a -> b -> c -> d -> e -> m x)) ~ m) => Bind4 (a -> b -> c -> d -> e -> m x) where
    bind4 f' my a b c d e f g h = do
        y <- my
        f' a b c y d e f g h

instance {-# OVERLAPPABLE #-} (Monad m, UnaryType (Result (a -> b -> c -> d -> e -> f -> m x)) ~ m) => Bind4 (a -> b -> c -> d -> e -> f -> m x) where
    bind4 f' my a b c d e f g h i = do
        y <- my
        f' a b c y d e f g h i

class Monad (UnaryType (Result rest)) => Bind5 rest where
    bind5 :: (a -> b -> c -> d -> e -> rest) -> (UnaryType (Result rest)) e -> a -> b -> c -> d -> rest

instance {-# OVERLAPPABLE #-} (Monad m, UnaryType (Result (m x)) ~ m) => Bind5 (m x) where
    bind5 f' my a b c d = do
        y <- my
        f' a b c d y

instance {-# OVERLAPPABLE #-} (Monad m, UnaryType (Result (a -> m x)) ~ m) => Bind5 (a -> m x) where
    bind5 f' my a b c d e = do
        y <- my
        f' a b c d y e

instance {-# OVERLAPPABLE #-} (Monad m, UnaryType (Result (a -> b -> m x)) ~ m) => Bind5 (a -> b -> m x) where
    bind5 f' my a b c d e f = do
        y <- my
        f' a b c d y e f

instance {-# OVERLAPPABLE #-} (Monad m, UnaryType (Result (a -> b -> c -> m x)) ~ m) => Bind5 (a -> b -> c -> m x) where
    bind5 f' my a b c d e f g = do
        y <- my
        f' a b c d y e f g

instance {-# OVERLAPPABLE #-} (Monad m, UnaryType (Result (a -> b -> c -> d -> m x)) ~ m) => Bind5 (a -> b -> c -> d -> m x) where
    bind5 f' my a b c d e f g h = do
        y <- my
        f' a b c d y e f g h

instance {-# OVERLAPPABLE #-} (Monad m, UnaryType (Result (a -> b -> c -> d -> e -> m x)) ~ m) => Bind5 (a -> b -> c -> d -> e -> m x) where
    bind5 f' my a b c d e f g h i = do
        y <- my
        f' a b c d y e f g h i

class Monad (UnaryType (Result rest)) => Bind6 rest where
    bind6 :: (a -> b -> c -> d -> e -> f -> rest) -> (UnaryType (Result rest)) f -> a -> b -> c -> d -> e -> rest

instance {-# OVERLAPPABLE #-} (Monad m, UnaryType (Result (m x)) ~ m) => Bind6 (m x) where
    bind6 f' my a b c d e = do
        y <- my
        f' a b c d e y

instance {-# OVERLAPPABLE #-} (Monad m, UnaryType (Result (a -> m x)) ~ m) => Bind6 (a -> m x) where
    bind6 f' my a b c d e f = do
        y <- my
        f' a b c d e y f

instance {-# OVERLAPPABLE #-} (Monad m, UnaryType (Result (a -> b -> m x)) ~ m) => Bind6 (a -> b -> m x) where
    bind6 f' my a b c d e f g = do
        y <- my
        f' a b c d e y f g

instance {-# OVERLAPPABLE #-} (Monad m, UnaryType (Result (a -> b -> c -> m x)) ~ m) => Bind6 (a -> b -> c -> m x) where
    bind6 f' my a b c d e f g h = do
        y <- my
        f' a b c d e y f g h

instance {-# OVERLAPPABLE #-} (Monad m, UnaryType (Result (a -> b -> c -> d -> m x)) ~ m) => Bind6 (a -> b -> c -> d -> m x) where
    bind6 f' my a b c d e f g h i = do
        y <- my
        f' a b c d e y f g h i

class Monad (UnaryType (Result rest)) => Bind7 rest where
    bind7 :: (a -> b -> c -> d -> e -> f -> g -> rest) -> (UnaryType (Result rest)) g -> a -> b -> c -> d -> e -> f -> rest

instance {-# OVERLAPPABLE #-} (Monad m, UnaryType (Result (m x)) ~ m) => Bind7 (m x) where
    bind7 f' my a b c d e f = do
        y <- my
        f' a b c d e f y

instance {-# OVERLAPPABLE #-} (Monad m, UnaryType (Result (a -> m x)) ~ m) => Bind7 (a -> m x) where
    bind7 f' my a b c d e f g = do
        y <- my
        f' a b c d e f y g

instance {-# OVERLAPPABLE #-} (Monad m, UnaryType (Result (a -> b -> m x)) ~ m) => Bind7 (a -> b -> m x) where
    bind7 f' my a b c d e f g h = do
        y <- my
        f' a b c d e f y g h

instance {-# OVERLAPPABLE #-} (Monad m, UnaryType (Result (a -> b -> c -> m x)) ~ m) => Bind7 (a -> b -> c -> m x) where
    bind7 f' my a b c d e f g h i = do
        y <- my
        f' a b c d e f y g h i

class Monad (UnaryType (Result rest)) => Bind8 rest where
    bind8 :: (a -> b -> c -> d -> e -> f -> g -> h -> rest) -> (UnaryType (Result rest)) h -> a -> b -> c -> d -> e -> f -> g -> rest

instance {-# OVERLAPPABLE #-} (Monad m, UnaryType (Result (m x)) ~ m) => Bind8 (m x) where
    bind8 f' my a b c d e f g = do
        y <- my
        f' a b c d e f g y

instance {-# OVERLAPPABLE #-} (Monad m, UnaryType (Result (a -> m x)) ~ m) => Bind8 (a -> m x) where
    bind8 f' my a b c d e f g h = do
        y <- my
        f' a b c d e f g y h

instance {-# OVERLAPPABLE #-} (Monad m, UnaryType (Result (a -> b -> m x)) ~ m) => Bind8 (a -> b -> m x) where
    bind8 f' my a b c d e f g h i = do
        y <- my
        f' a b c d e f g y h i


class Monad (UnaryType (Result rest)) => Bind9 rest where
    bind9 :: (a -> b -> c -> d -> e -> f -> g -> h -> i -> rest) -> (UnaryType (Result rest)) i -> a -> b -> c -> d -> e -> f -> g -> h -> rest

instance {-# OVERLAPPABLE #-} (Monad m, UnaryType (Result (m x)) ~ m) => Bind9 (m x) where
    bind9 f' my a b c d e f g h = do
        y <- my
        f' a b c d e f g h y

instance {-# OVERLAPPABLE #-} (Monad m, UnaryType (Result (a -> m x)) ~ m) => Bind9 (a -> m x) where
    bind9 f' my a b c d e f g h i = do
        y <- my
        f' a b c d e f g h y i

class Monad (UnaryType (Result rest)) => Bind10 rest where
    bind10 :: (a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> rest) -> (UnaryType (Result rest)) j -> a -> b -> c -> d -> e -> f -> g -> h -> i -> rest

instance {-# OVERLAPPABLE #-} (Monad m, UnaryType (Result (m x)) ~ m) => Bind10 (m x) where
    bind10 f' my a b c d e f g h i = do
        y <- my
        f' a b c d e f g h i y
