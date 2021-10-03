module Lib4 where

import Prelude hiding (lookup)

class MapLike m where
  empty :: m k v
  lookup :: Ord k => k -> m k v -> Maybe v
  insert :: Ord k => k -> v -> m k v -> m k v
  delete :: Ord k => k -> m k v -> m k v
  fromList :: Ord k => [(k, v)] -> m k v

newtype ArrowMap k v = ArrowMap {getArrowMap :: k -> Maybe v}

check :: Ord k => (k, v) -> (k -> Maybe v)
check (x, y) k = if x == k then Just y else Nothing

fl :: Ord k => (k, v) -> (k -> Maybe v) -> (k -> Maybe v)
fl pair a m = case a m of
  Nothing -> check pair m
  Just v -> Just v

instance MapLike ArrowMap where
  empty = ArrowMap $ const Nothing
  lookup k (ArrowMap xs) = xs k
  insert k v (ArrowMap xs) = ArrowMap $ fl (k, v) xs
  delete k (ArrowMap xs) = ArrowMap $ \m -> if m == k then Nothing else xs m
  fromList xs = ArrowMap $ foldr fl (const Nothing) xs
