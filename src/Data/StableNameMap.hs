{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}

module Data.StableNameMap where

import Control.DeepSeq
import qualified Data.DList as DL
import qualified Data.Foldable as F
import Data.Functor.Classes
import Data.Functor.Identity
import qualified Data.IntMap.Strict as M
import qualified Data.List as L
import Data.Maybe
import qualified GHC.Exts as G
import GHC.StableName

-- | There is no instance of Ord as the traversal a StableNameMap is not stable
newtype StableNameMap k a = StableNameMap (M.IntMap [(StableName k, a)])

-- | The order of the fold is not stable - it depends on insertion order.
-- Two equivalent StableNameMaps may have different toList ordering
instance Foldable (StableNameMap k) where
  foldr f b (StableNameMap m) = M.foldr go b m
    where
      go xs b' = F.foldr go2 b' xs
      go2 (_, a) b'' = f a b''

instance Functor (StableNameMap k) where
  fmap f (StableNameMap m) = StableNameMap $ (fmap . fmap $ fmap f) m

instance Eq a => Eq (StableNameMap k a) where
  (==) = liftEq (==)

instance Eq1 (StableNameMap k) where
  liftEq f (StableNameMap m) (StableNameMap n) = go (M.toList m) (M.toList n)
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

instance G.IsList (StableNameMap k a) where
  type Item (StableNameMap k a) = (StableName k, a)
  fromList = fromList
  toList = toList

instance NFData1 (StableNameMap k) where
  liftRnf f (StableNameMap m) = F.foldl' go () m
    where
      go () xs = F.foldl' go2 () xs
      go2 () (_, a) = () `seq` f a

instance NFData a => NFData (StableNameMap k a) where
  rnf = liftRnf rnf

-- Construction

empty :: StableNameMap k a
empty = StableNameMap $ M.empty

singleton :: StableName k -> a -> StableNameMap k a
singleton k a = StableNameMap $ M.singleton (hashStableName k) [(k, a)]

-- From Unordered List

fromList :: [(StableName k, a)] -> StableNameMap k a
fromList = F.foldr go empty
  where
    go (k, a) = insert k a

fromListWith :: (a -> a -> a) -> [(StableName k, a)] -> StableNameMap k a
fromListWith f = F.foldr go empty
  where
    go (k, a) = insertWith f k a

fromListWithKey :: (StableName k -> a -> a -> a) -> [(StableName k, a)] -> StableNameMap k a
fromListWithKey f = F.foldr go empty
  where
    go (k, a) = insertWithKey f k a

-- Insertion

insert :: StableName k -> a -> StableNameMap k a -> StableNameMap k a
insert = insertWith const

insertWith :: (a -> a -> a) -> StableName k -> a -> StableNameMap k a -> StableNameMap k a
insertWith f = insertWithKey (const f)

insertWithKey :: (StableName k -> a -> a -> a) -> StableName k -> a -> StableNameMap k a -> StableNameMap k a
insertWithKey f k a = snd . insertLookupWithKey f k a

insertLookupWithKey :: (StableName k -> a -> a -> a) -> StableName k -> a -> StableNameMap k a -> (Maybe a, StableNameMap k a)
insertLookupWithKey f k a (StableNameMap m) =
    let (ma, m') = M.insertLookupWithKey go (hashStableName k) [(k, a)] m
    in (ma >>= F.foldr go3 Nothing, StableNameMap m')
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

delete :: StableName k -> StableNameMap k a -> StableNameMap k a
delete = update (const Nothing)

adjust :: (a -> a) -> StableName k -> StableNameMap k a -> StableNameMap k a
adjust f = adjustWithKey (const f)

adjustWithKey :: (StableName k -> a -> a) -> StableName k -> StableNameMap k a -> StableNameMap k a
adjustWithKey f k (StableNameMap m) = StableNameMap (M.adjust (fmap go) (hashStableName k) m)
  where
    go a@(k', v) = if k == k' then (k', f k' v) else a

update :: (a -> Maybe a) -> StableName k -> StableNameMap k a -> StableNameMap k a
update f = updateWithKey (const f)

updateWithKey :: (StableName k -> a -> Maybe a) -> StableName k -> StableNameMap k a -> StableNameMap k a
updateWithKey f k = snd . updateLookupWithKey f k

updateLookupWithKey :: (StableName k -> a -> Maybe a) -> StableName k -> StableNameMap k a -> (Maybe a, StableNameMap k a)
updateLookupWithKey f k (StableNameMap m) =
    let (ma, m') = M.updateLookupWithKey go (hashStableName k) m
    in (ma >>= F.foldr go3 Nothing, StableNameMap m')
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

alter :: (Maybe a -> Maybe a) -> StableName k -> StableNameMap k a -> StableNameMap k a
alter f k = runIdentity . alterF (Identity . f) k

alterF :: Functor f => (Maybe a -> f (Maybe a)) -> StableName k -> StableNameMap k a -> f (StableNameMap k a)
alterF f k (StableNameMap m) = StableNameMap <$> (M.alterF go (hashStableName k) m)
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

null :: StableNameMap k a -> Bool
null m = size m == 0

size :: StableNameMap k a -> Int
size (StableNameMap m) = F.foldr (\xs i -> length xs + i) 0 m

-- Combine
-- Union

unionWith :: (a -> a -> a) -> StableNameMap k a -> StableNameMap k a -> StableNameMap k a
unionWith f = unionWithKey (const f)

unionWithKey :: (StableName k -> a -> a -> a) -> StableNameMap k a -> StableNameMap k a -> StableNameMap k a
unionWithKey f m n = F.foldl' go m (toList n)
  where
    go m' (k, a) = insertWithKey f k a m'

unions :: Foldable f => f (StableNameMap k a) -> StableNameMap k a
unions = unionsWith const

unionsWith :: Foldable f => (a -> a -> a) -> f (StableNameMap k a) -> StableNameMap k a
unionsWith f xs = F.foldl' go Data.StableNameMap.empty xs
  where
    go m' n = unionWith f m' n

-- Traversl
-- Map
map :: (a -> b) -> StableNameMap k a -> StableNameMap k b
map f (StableNameMap m) = StableNameMap $ M.map (fmap (fmap f)) m

mapWithKey :: (StableName k -> a -> b) -> StableNameMap k a -> StableNameMap k b
mapWithKey f (StableNameMap m) = StableNameMap $ M.map (fmap go) m
  where
    go (k, a) = (k, f k a)

traverseWithKey :: Applicative t => (StableName k -> a -> t b) -> StableNameMap k a -> t (StableNameMap k b)
traverseWithKey f (StableNameMap m) = StableNameMap <$> traverse (traverse go) m
  where
    go (k, a) = (\b -> (k, b)) <$> f k a

mapAccum :: (a -> b -> (a, c)) -> a -> StableNameMap k b -> (a, StableNameMap k c)
mapAccum f = mapAccumWithKey (\a _ b -> f a b)

mapAccumWithKey :: (a -> StableName k -> b -> (a, c)) -> a -> StableNameMap k b -> (a, StableNameMap k c)
mapAccumWithKey f a (StableNameMap m) =
    let (a', m') = M.mapAccumWithKey go a m
    in (a', StableNameMap m')
  where
    go ac _ bs = let (ac', xs) = (F.foldl' go2 (ac, mempty) bs) in (ac', DL.toList xs)
    go2 (ac, xs) (k, b) = let (ac', c) = f ac k b in (ac', (k, c) `DL.cons` xs)

mapAccumRWithKey :: (a -> StableName k -> b -> (a, c)) -> a -> StableNameMap k b -> (a, StableNameMap k c)
mapAccumRWithKey f a (StableNameMap m) =
    let (a', m') = M.mapAccumRWithKey go a m
    in (a', StableNameMap m')
  where
    go ac _ bs = F.foldr go2 (ac, []) bs
    go2 (k, b) (ac, xs) = let (ac', c) = f ac k b in (ac', (k, c) : xs)

mapKeys :: (StableName k -> StableName k) -> StableNameMap k a -> StableNameMap k a
mapKeys f m = fromList $ (\(k, a) -> (f k, a)) <$> toList m

mapKeysWith :: (a -> a -> a) -> (StableName k -> StableName k) -> StableNameMap k a -> StableNameMap k a
mapKeysWith c f m = fromListWith c $ (\(k, a) -> (f k, a)) <$> toList m

-- Folds

foldr :: (a -> b -> b) -> b -> StableNameMap k a -> b
foldr f = foldrWithKey (const f)

foldl :: (b -> a -> b) -> b -> StableNameMap k a -> b
foldl f = foldlWithKey (\a _ b -> f a b)

foldrWithKey :: (StableName k -> a -> b -> b) -> b -> StableNameMap k a -> b
foldrWithKey f b (StableNameMap m) = F.foldr go b m
  where
    go as b' = F.foldr go2 b' as
    go2 (k, a) b' = f k a b'

foldlWithKey :: (a -> StableName k -> b -> a) -> a -> StableNameMap k b -> a
foldlWithKey f b (StableNameMap m) = F.foldl go b m
  where
    go b' as = F.foldl go2 b' as
    go2 b' (k, a) = f b' k a

foldMapWithKey :: Monoid m => (StableName k -> a -> m) -> StableNameMap k a -> m
foldMapWithKey f (StableNameMap m) = M.foldMapWithKey go m
  where
    go _ as = F.foldMap go2 as
    go2 (k, a) = f k a

-- Strict folds

foldr' :: (a -> b -> b) -> b -> StableNameMap k a -> b
foldr' f = foldrWithKey' (const f)

foldl' :: (b -> a -> b) -> b -> StableNameMap k a -> b
foldl' f = foldlWithKey' (\a _ b -> f a b)

foldrWithKey' :: (StableName k -> a -> b -> b) -> b -> StableNameMap k a -> b
foldrWithKey' f b (StableNameMap m) = F.foldr go b m
  where
    go as b' = F.foldr go2 b' as
    go2 (k, a) !b' = f k a b'

foldlWithKey' :: (a -> StableName k -> b -> a) -> a -> StableNameMap k b -> a
foldlWithKey' f b (StableNameMap m) = F.foldl go b m
  where
    go b' as = F.foldl go2 b' as
    go2 !b' (k, a) = f b' k a

-- Conversion
elems :: StableNameMap k a -> [a]
elems (StableNameMap m) = foldMap (fmap snd) $ M.elems m

keys :: StableNameMap k a -> [StableName k]
keys (StableNameMap m) = foldMap (fmap fst) $ M.elems m

-- Lists

-- | The order of toList is not stable - it depends om insertion order
-- Two equivalent StableNameMaps may have different toList ordering
toList :: StableNameMap k a -> [(StableName k, a)]
toList (StableNameMap m) = DL.toList (F.foldr go mempty (M.toList m))
  where
    go (_, as) xs = (DL.fromList as) <> xs

-- Filter
filter :: (a -> Bool) -> StableNameMap k a -> StableNameMap k a
filter f = filterWithKey (const f)

filterWithKey :: (StableName k -> a -> Bool) -> StableNameMap k a -> StableNameMap k a
filterWithKey f (StableNameMap m) = StableNameMap $ M.mapMaybe go m
  where
    go xs = case L.filter go2 xs of
      [] -> Nothing
      xs' -> Just xs'
    go2 (k, a) = f k a

restrictKeys :: StableNameMap k a -> [StableName k] -> StableNameMap k a
restrictKeys (StableNameMap m) ks =
    let ks' = M.fromListWith (<>) ((\k -> (hashStableName k, [k])) <$> ks)
        go k xs = case M.lookup k ks' of
            Nothing -> Nothing
            Just ks'' -> case L.filter (\(k', _) -> L.elem k' ks'') xs of
              [] -> Nothing
              xs' -> Just xs'
    in StableNameMap $ M.mapMaybeWithKey go m

withoutKeys :: StableNameMap k a -> [StableName k] -> StableNameMap k a
withoutKeys (StableNameMap m) ks =
    let ks' = M.fromListWith (<>) ((\k -> (hashStableName k, [k])) <$> ks)
        go k xs = case M.lookup k ks' of
            Nothing -> Just xs
            Just ks'' -> case L.filter (\(k', _) -> not $ L.elem k' ks'') xs of
              [] -> Nothing
              xs' -> Just xs'
    in StableNameMap $ M.mapMaybeWithKey go m

partition :: (a -> Bool) -> StableNameMap k a -> (StableNameMap k a, StableNameMap k a)
partition f = partitionWithKey (const f)

partitionWithKey :: (StableName k -> a -> Bool) -> StableNameMap k a -> (StableNameMap k a, StableNameMap k a)
partitionWithKey f m = let (as, bs) = L.partition (\(k, a) -> f k a) $ toList m in (fromList as, fromList bs)

mapMaybe :: (a -> Maybe b) -> StableNameMap k a -> StableNameMap k b
mapMaybe f = mapMaybeWithKey (const f)

mapMaybeWithKey :: (StableName k -> a -> Maybe b) -> StableNameMap k a -> StableNameMap k b
mapMaybeWithKey f (StableNameMap m) = StableNameMap $ M.mapMaybe go m
  where
    go xs = case catMaybes $ (\(k, a) -> (\b -> (k, b)) <$> f k a) <$> xs of
      [] -> Nothing
      xs' -> Just xs'

mapEither :: (a -> Either b c) -> StableNameMap k a -> (StableNameMap k b, StableNameMap k c)
mapEither f = mapEitherWithKey (const f)

mapEitherWithKey :: (StableName k -> a -> Either b c) -> StableNameMap k a -> (StableNameMap k b, StableNameMap k c)
mapEitherWithKey f m = let (xs, ys) = F.foldr go ([], []) (toList m) in (fromList xs, fromList ys)
  where
    go (k, a) (xs, ys) = case f k a of
      Left x -> ((k, x) : xs, ys)
      Right y -> (xs, (k, y) : ys)

