module Module5 where

import Data.Char (toUpper)

-- Определите представителя класса Functor для следующего типа данных, представляющего точку в трёхмерном пространстве:

data Point3D a = Point3D a a a deriving (Show)

instance Functor Point3D where
  fmap f (Point3D x y z) = Point3D (f x) (f y) (f z)

-- >>> fmap (+ 1) (Point3D 5 6 7)
-- Point3D 6 7 8

{-  -}

-- Определите представителя класса Functor для типа данных GeomPrimitive, который определён следующим образом:

data GeomPrimitive a = Point (Point3D a) | LineSegment (Point3D a) (Point3D a)

-- При определении, воспользуйтесь тем, что Point3D уже является представителем класса Functor.

-- >>> fmap (+ 1) $ Point (Point3D 0 0 0)
-- Point (Point3D 1 1 1)

-- >>> fmap (+ 1) $ LineSegment (Point3D 0 0 0) (Point3D 1 1 1)
-- LineSegment (Point3D 1 1 1) (Point3D 2 2 2)

instance Functor GeomPrimitive where
  fmap f (Point x) = Point $ fmap f x
  fmap f (LineSegment x y) = LineSegment (fmap f x) (fmap f y)

{-  -}

-- Определите представителя класса Functor для бинарного дерева, в каждом узле которого хранятся элементы типа Maybe:

data Tree a = Leaf (Maybe a) | Branch (Tree a) (Maybe a) (Tree a) deriving (Show)

-- >>> words <$> Leaf Nothing
-- Leaf Nothing

-- >>> words <$> Leaf (Just "a b")
-- Leaf (Just ["a","b"])

instance Functor Tree where
  fmap _ (Leaf Nothing) = Leaf Nothing
  fmap f (Leaf (Just x)) = Leaf $ Just $ f x
  fmap f (Branch l Nothing r) = Branch (fmap f l) Nothing (fmap f r)
  fmap f (Branch l (Just v) r) = Branch (fmap f l) (Just $ f v) (fmap f r)

{-  -}

data Entry k1 k2 v = Entry (k1, k2) v deriving (Show)

data Map k1 k2 v = Map [Entry k1 k2 v] deriving (Show)

-- >>> fmap (map toUpper) $ Map []
-- Map []
-- >>> fmap (map toUpper) $ Map [Entry (0, 0) "origin", Entry (800, 0) "right corner"]
-- Map [Entry (0,0) "ORIGIN",Entry (800,0) "RIGHT CORNER"]

instance Functor (Entry k1 k2) where
  fmap f (Entry k v) = Entry k $ f v

instance Functor (Map k1 k2) where
  fmap f (Map xs) = Map $ map (fmap f) xs
