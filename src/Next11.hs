module Next11 where

newtype Arr2 e1 e2 a = Arr2 {getArr2 :: e1 -> e2 -> a}

newtype Arr3 e1 e2 e3 a = Arr3 {getArr3 :: e1 -> e2 -> e3 -> a}

-- >>> :t take
-- take :: Int -> [a] -> [a]

-- >>> :t zipWith
-- zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]

-- >>> getArr2 (fmap length (Arr2 take)) 10 "abc"
-- 3

-- >>> getArr3 (tail <$> tail <$> Arr3 zipWith) (+) [1,2,3,4] [10,20,30,40,50]
-- [33,44]

instance Functor (Arr2 e1 e2) where
  fmap f (Arr2 a) = Arr2 (\e1 e2 -> f $ a e1 e2)

instance Functor (Arr3 e1 e2 e3) where
  fmap f (Arr3 a) = Arr3 (\e1 e2 e3 -> f $ a e1 e2 e3)
