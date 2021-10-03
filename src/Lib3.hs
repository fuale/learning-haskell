module Lib3 where

import qualified Data.List as L
import Prelude hiding (lookup)

class MapLike m where
  empty :: m k v
  lookup :: Ord k => k -> m k v -> Maybe v
  insert :: Ord k => k -> v -> m k v -> m k v
  delete :: Ord k => k -> m k v -> m k v
  fromList :: Ord k => [(k, v)] -> m k v
  fromList [] = empty
  fromList ((k, v) : xs) = insert k v (fromList xs)

newtype ListMap k v = ListMap {getListMap :: [(k, v)]}
  deriving (Eq, Show)

instance MapLike ListMap where
  empty = ListMap []
  lookup k (ListMap (x : xs)) = if fst x == k then Just $ snd x else lookup k (ListMap xs)
  lookup k (ListMap []) = Nothing
  insert k v xx@(ListMap xs) = case lookup k xx of
    Nothing -> ListMap $ (k, v) : xs
    Just v' -> ListMap $ map (\r@(key, _) -> if key == k then (k, v) else r) xs
  delete k (ListMap xs) = ListMap $ filter (\(key, _) -> k /= key) xs
