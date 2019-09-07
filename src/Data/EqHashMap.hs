{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}

module Data.EqHashMap where

import Control.DeepSeq
import qualified Data.DList as DL
import qualified Data.Foldable as F
import Data.Functor.Classes
import Data.Hashable
import Data.Functor.Identity
import qualified Data.IntMap.Strict as M
import qualified Data.List as L
import Data.Maybe
import qualified GHC.Exts as G

-- | A hashmap requring only 'Eq' and 'Hashable' on the key, allowing for duplicate entries.
-- There is no instance of Ord as the traversal of a EqHashMap is not stable
newtype EqHashMap k a = EqHashMap (M.IntMap [(k, a)])

-- | The order of the fold is not stable - it depends on insertion order.
-- Two equivalent EqHashMaps may have different toList ordering
instance Foldable (EqHashMap k) where
  foldr f b (EqHashMap m) = M.foldr go b m
    where
      go xs b' = F.foldr go2 b' xs
      go2 (_, a) b'' = f a b''

instance Functor (EqHashMap k) where
  fmap f (EqHashMap m) = EqHashMap $ (fmap . fmap $ fmap f) m

instance (Eq a, Eq k) => Eq (EqHashMap k a) where
  (==) = liftEq (==)

instance Eq k => Eq1 (EqHashMap k) where
  liftEq f (EqHashMap m) (EqHashMap n) = go (M.toList m) (M.toList n)
    where
      go ((k1, as) : xs) ((k2, bs) : ys) =
        -- both IntMap should have Keys in the same order
        if (k1 /= k2)
          then False
          else if go2 as bs
            then go xs ys
            else False
      go _ _ = False

      go2 as bs = case F.foldr go3 (Just bs) as of
        Nothing -> False
        Just [] -> True
        Just _ -> False

      go3 _ Nothing = Nothing
      go3 _ (Just []) = Nothing
      go3 a (Just bs) = Just (delete' a bs)

      delete'  _ [] = []
      delete' a@(k1, x) (b@(k2, y) : ys) =
        if k1 == k2 && f x y
          then ys
          else b : delete' a ys

instance (Eq k, Hashable k) => G.IsList (EqHashMap k a) where
  type Item (EqHashMap k a) = (k, a)
  fromList = fromList
  toList = toList

instance NFData1 (EqHashMap k) where
  liftRnf f (EqHashMap m) = F.foldl' go () m
    where
      go () xs = F.foldl' go2 () xs
      go2 () (_, a) = () `seq` f a

instance NFData a => NFData (EqHashMap k a) where
  rnf = liftRnf rnf

-- Construction

empty :: EqHashMap k a
empty = EqHashMap $ M.empty

singleton :: Hashable k => k -> a -> EqHashMap k a
singleton k a = EqHashMap $ M.singleton (hash k) [(k, a)]

-- From Unordered List

fromList :: (Eq k, Hashable k) => [(k, a)] -> EqHashMap k a
fromList = F.foldr go empty
  where
    go (k, a) = insert k a

fromListWith :: (Eq k, Hashable k) => (a -> a -> a) -> [(k, a)] -> EqHashMap k a
fromListWith f = F.foldr go empty
  where
    go (k, a) = insertWith f k a

fromListWithKey :: (Eq k, Hashable k) => (k -> a -> a -> a) -> [(k, a)] -> EqHashMap k a
fromListWithKey f = F.foldr go empty
  where
    go (k, a) = insertWithKey f k a

-- Insertion

insert :: (Eq k, Hashable k) => k -> a -> EqHashMap k a -> EqHashMap k a
insert = insertWith const

insertWith :: (Eq k, Hashable k) => (a -> a -> a) -> k -> a -> EqHashMap k a -> EqHashMap k a
insertWith f = insertWithKey (const f)

insertWithKey :: (Eq k, Hashable k) => (k -> a -> a -> a) -> k -> a -> EqHashMap k a -> EqHashMap k a
insertWithKey f k a = snd . insertLookupWithKey f k a

insertLookupWithKey :: (Eq k, Hashable k) => (k -> a -> a -> a) -> k -> a -> EqHashMap k a -> (Maybe a, EqHashMap k a)
insertLookupWithKey f k a (EqHashMap m) =
    let (ma, m') = M.insertLookupWithKey go (hash k) [(k, a)] m
    in (ma >>= F.foldr go3 Nothing, EqHashMap m')
  where
    go _ _ old_xs = case F.foldr go2 (False, []) old_xs of
      (False, xs) -> (k, a) : xs
      (True, xs) -> xs

    go2 old@(oldK, oldA) (found, xs) =
      if (not found) && (k == oldK)
        then (True, (k, f k a oldA) : xs)
        else (found, old : xs)

    go3 _ (Just x) = Just x
    go3 (oldK, oldA) Nothing = if k == oldK then Just oldA else Nothing

-- Deletion/Update

delete :: (Eq k, Hashable k) => k -> EqHashMap k a -> EqHashMap k a
delete = update (const Nothing)

adjust :: (Eq k, Hashable k) => (a -> a) -> k -> EqHashMap k a -> EqHashMap k a
adjust f = adjustWithKey (const f)

adjustWithKey :: (Eq k, Hashable k) => (k -> a -> a) -> k -> EqHashMap k a -> EqHashMap k a
adjustWithKey f k (EqHashMap m) = EqHashMap (M.adjust (fmap go) (hash k) m)
  where
    go a@(k', v) = if k == k' then (k', f k' v) else a

update :: (Eq k, Hashable k) => (a -> Maybe a) -> k -> EqHashMap k a -> EqHashMap k a
update f = updateWithKey (const f)

updateWithKey :: (Eq k, Hashable k) => (k -> a -> Maybe a) -> k -> EqHashMap k a -> EqHashMap k a
updateWithKey f k = snd . updateLookupWithKey f k

updateLookupWithKey :: (Eq k, Hashable k) => (k -> a -> Maybe a) -> k -> EqHashMap k a -> (Maybe a, EqHashMap k a)
updateLookupWithKey f k (EqHashMap m) =
    let (ma, m') = M.updateLookupWithKey go (hash k) m
    in (ma >>= F.foldr go3 Nothing, EqHashMap m')
  where
    go _ old_xs = case F.foldr go2 [] old_xs of
      [] -> Nothing
      xs -> Just xs

    go2 old@(oldK, oldA) xs =
      if k == oldK
        then case f k oldA of
          Nothing -> xs
          Just x -> (k, x) : xs
        else old : xs

    go3 _ (Just x) = Just x
    go3 (oldK, oldA) Nothing = if k == oldK then Just oldA else Nothing

alter :: (Eq k, Hashable k) => (Maybe a -> Maybe a) -> k -> EqHashMap k a -> EqHashMap k a
alter f k = runIdentity . alterF (Identity . f) k

alterF :: (Functor f, Eq k, Hashable k) => (Maybe a -> f (Maybe a)) -> k -> EqHashMap k a -> f (EqHashMap k a)
alterF f k (EqHashMap m) = EqHashMap <$> (M.alterF go (hash k) m)
  where
    go Nothing = (fmap (\x -> [(k, x)])) <$> f Nothing

    go (Just xs) = case F.foldr go3 (Nothing, []) xs of
      (Just fma, xs') -> (go2 xs') <$> fma
      (Nothing, xs') -> (go2 xs') <$> f Nothing

    go2 xs' Nothing = Just xs'
    go2 xs' (Just x) = Just ((k, x) : xs')

    go3 a@(k', v) (Nothing, xs') =
      if (k' == k)
        then (Just . f $ Just v, xs')
      else (Nothing, a : xs')
    go3 a (jma, xs') = (jma, a : xs')

-- Size

null :: EqHashMap k a -> Bool
null m = size m == 0

size :: EqHashMap k a -> Int
size (EqHashMap m) = F.foldr (\xs i -> length xs + i) 0 m

-- Combine
-- Union

unionWith :: (Eq k, Hashable k) => (a -> a -> a) -> EqHashMap k a -> EqHashMap k a -> EqHashMap k a
unionWith f = unionWithKey (const f)

unionWithKey :: (Eq k, Hashable k) => (k -> a -> a -> a) -> EqHashMap k a -> EqHashMap k a -> EqHashMap k a
unionWithKey f m n = F.foldl' go m (toList n)
  where
    go m' (k, a) = insertWithKey f k a m'

unions :: (Foldable f, Eq k, Hashable k) => f (EqHashMap k a) -> EqHashMap k a
unions = unionsWith const

unionsWith :: (Foldable f, Eq k, Hashable k) => (a -> a -> a) -> f (EqHashMap k a) -> EqHashMap k a
unionsWith f xs = F.foldl' go Data.EqHashMap.empty xs
  where
    go m' n = unionWith f m' n

-- Traversl
-- Map
map :: (a -> b) -> EqHashMap k a -> EqHashMap k b
map f (EqHashMap m) = EqHashMap $ M.map (fmap (fmap f)) m

mapWithKey :: (k -> a -> b) -> EqHashMap k a -> EqHashMap k b
mapWithKey f (EqHashMap m) = EqHashMap $ M.map (fmap go) m
  where
    go (k, a) = (k, f k a)

traverseWithKey :: Applicative t => (k -> a -> t b) -> EqHashMap k a -> t (EqHashMap k b)
traverseWithKey f (EqHashMap m) = EqHashMap <$> traverse (traverse go) m
  where
    go (k, a) = (\b -> (k, b)) <$> f k a

mapAccum :: (a -> b -> (a, c)) -> a -> EqHashMap k b -> (a, EqHashMap k c)
mapAccum f = mapAccumWithKey (\a _ b -> f a b)

mapAccumWithKey :: (a -> k -> b -> (a, c)) -> a -> EqHashMap k b -> (a, EqHashMap k c)
mapAccumWithKey f a (EqHashMap m) =
    let (a', m') = M.mapAccumWithKey go a m
    in (a', EqHashMap m')
  where
    go ac _ bs = let (ac', xs) = (F.foldl' go2 (ac, mempty) bs) in (ac', DL.toList xs)
    go2 (ac, xs) (k, b) = let (ac', c) = f ac k b in (ac', (k, c) `DL.cons` xs)

mapAccumRWithKey :: (a -> k -> b -> (a, c)) -> a -> EqHashMap k b -> (a, EqHashMap k c)
mapAccumRWithKey f a (EqHashMap m) =
    let (a', m') = M.mapAccumRWithKey go a m
    in (a', EqHashMap m')
  where
    go ac _ bs = F.foldr go2 (ac, []) bs
    go2 (k, b) (ac, xs) = let (ac', c) = f ac k b in (ac', (k, c) : xs)

mapKeys :: (Eq k, Hashable k) => (k -> k) -> EqHashMap k a -> EqHashMap k a
mapKeys f m = fromList $ (\(k, a) -> (f k, a)) <$> toList m

mapKeysWith :: (Eq k, Hashable k) => (a -> a -> a) -> (k -> k) -> EqHashMap k a -> EqHashMap k a
mapKeysWith c f m = fromListWith c $ (\(k, a) -> (f k, a)) <$> toList m

-- Folds

foldr :: (a -> b -> b) -> b -> EqHashMap k a -> b
foldr f = foldrWithKey (const f)

foldl :: (b -> a -> b) -> b -> EqHashMap k a -> b
foldl f = foldlWithKey (\a _ b -> f a b)

foldrWithKey :: (k -> a -> b -> b) -> b -> EqHashMap k a -> b
foldrWithKey f b (EqHashMap m) = F.foldr go b m
  where
    go as b' = F.foldr go2 b' as
    go2 (k, a) b' = f k a b'

foldlWithKey :: (a -> k -> b -> a) -> a -> EqHashMap k b -> a
foldlWithKey f b (EqHashMap m) = F.foldl go b m
  where
    go b' as = F.foldl go2 b' as
    go2 b' (k, a) = f b' k a

foldMapWithKey :: Monoid m => (k -> a -> m) -> EqHashMap k a -> m
foldMapWithKey f (EqHashMap m) = M.foldMapWithKey go m
  where
    go _ as = F.foldMap go2 as
    go2 (k, a) = f k a

-- Strict folds

foldr' :: (a -> b -> b) -> b -> EqHashMap k a -> b
foldr' f = foldrWithKey' (const f)

foldl' :: (b -> a -> b) -> b -> EqHashMap k a -> b
foldl' f = foldlWithKey' (\a _ b -> f a b)

foldrWithKey' :: (k -> a -> b -> b) -> b -> EqHashMap k a -> b
foldrWithKey' f b (EqHashMap m) = F.foldr go b m
  where
    go as b' = F.foldr go2 b' as
    go2 (k, a) !b' = f k a b'

foldlWithKey' :: (a -> k -> b -> a) -> a -> EqHashMap k b -> a
foldlWithKey' f b (EqHashMap m) = F.foldl go b m
  where
    go b' as = F.foldl go2 b' as
    go2 !b' (k, a) = f b' k a

-- Conversion
elems :: EqHashMap k a -> [a]
elems (EqHashMap m) = foldMap (fmap snd) $ M.elems m

keys :: EqHashMap k a -> [k]
keys (EqHashMap m) = foldMap (fmap fst) $ M.elems m

-- Lists

-- | The order of toList is not stable - it depends om insertion order
-- Two equivalent EqHashMaps may have different toList ordering
toList :: EqHashMap k a -> [(k, a)]
toList (EqHashMap m) = DL.toList (F.foldr go mempty (M.toList m))
  where
    go (_, as) xs = (DL.fromList as) <> xs

-- Filter
filter :: (a -> Bool) -> EqHashMap k a -> EqHashMap k a
filter f = filterWithKey (const f)

filterWithKey :: (k -> a -> Bool) -> EqHashMap k a -> EqHashMap k a
filterWithKey f (EqHashMap m) = EqHashMap $ M.mapMaybe go m
  where
    go xs = case L.filter go2 xs of
      [] -> Nothing
      xs' -> Just xs'
    go2 (k, a) = f k a

restrictKeys :: (Eq k, Hashable k) => EqHashMap k a -> [k] -> EqHashMap k a
restrictKeys (EqHashMap m) ks =
    let ks' = M.fromListWith (<>) ((\k -> (hash k, [k])) <$> ks)
        go k xs = case M.lookup k ks' of
            Nothing -> Nothing
            Just ks'' -> case L.filter (\(k', _) -> L.elem k' ks'') xs of
              [] -> Nothing
              xs' -> Just xs'
    in EqHashMap $ M.mapMaybeWithKey go m

withoutKeys :: (Eq k, Hashable k) => EqHashMap k a -> [k] -> EqHashMap k a
withoutKeys (EqHashMap m) ks =
    let ks' = M.fromListWith (<>) ((\k -> (hash k, [k])) <$> ks)
        go k xs = case M.lookup k ks' of
            Nothing -> Just xs
            Just ks'' -> case L.filter (\(k', _) -> not $ L.elem k' ks'') xs of
              [] -> Nothing
              xs' -> Just xs'
    in EqHashMap $ M.mapMaybeWithKey go m

partition :: (Eq k, Hashable k) => (a -> Bool) -> EqHashMap k a -> (EqHashMap k a, EqHashMap k a)
partition f = partitionWithKey (const f)

partitionWithKey :: (Eq k, Hashable k) => (k -> a -> Bool) -> EqHashMap k a -> (EqHashMap k a, EqHashMap k a)
partitionWithKey f m = let (as, bs) = L.partition (\(k, a) -> f k a) $ toList m in (fromList as, fromList bs)

mapMaybe :: (a -> Maybe b) -> EqHashMap k a -> EqHashMap k b
mapMaybe f = mapMaybeWithKey (const f)

mapMaybeWithKey :: (k -> a -> Maybe b) -> EqHashMap k a -> EqHashMap k b
mapMaybeWithKey f (EqHashMap m) = EqHashMap $ M.mapMaybe go m
  where
    go xs = case catMaybes $ (\(k, a) -> (\b -> (k, b)) <$> f k a) <$> xs of
      [] -> Nothing
      xs' -> Just xs'

mapEither :: (Eq k, Hashable k) => (a -> Either b c) -> EqHashMap k a -> (EqHashMap k b, EqHashMap k c)
mapEither f = mapEitherWithKey (const f)

mapEitherWithKey :: (Eq k, Hashable k) => (k -> a -> Either b c) -> EqHashMap k a -> (EqHashMap k b, EqHashMap k c)
mapEitherWithKey f m = let (xs, ys) = F.foldr go ([], []) (toList m) in (fromList xs, fromList ys)
  where
    go (k, a) (xs, ys) = case f k a of
      Left x -> ((k, x) : xs, ys)
      Right y -> (xs, (k, y) : ys)

