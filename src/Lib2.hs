module Lib2 where

import Data.Bits
import qualified Data.Semigroup

data Nat = Zero | Suc Nat deriving (Show)

--- >>> fromNat (Suc (Suc (Suc Zero)))
-- 3
fromNat :: Nat -> Integer
fromNat Zero = 0
fromNat (Suc n) = fromNat n + 1

--- >>> toNat 4
-- Suc (Suc (Suc (Suc Zero)))
toNat :: Integer -> Nat
toNat x
  | x == 0 = Zero
  | x > 0 = Suc (toNat (x - 1))
  | otherwise = error "x can not be less than zero"

--- >>> add (Suc (Suc (Suc Zero))) (Suc (Suc (Suc Zero)))
-- Suc (Suc (Suc (Suc (Suc (Suc Zero)))))
add :: Nat -> Nat -> Nat
add x y = toNat (fromNat x + fromNat y)

--- >>> mul (Suc (Suc (Suc Zero))) (Suc (Suc (Suc Zero)))
-- Suc (Suc (Suc (Suc (Suc (Suc (Suc (Suc (Suc Zero))))))))
mul :: Nat -> Nat -> Nat
mul x y = toNat (fromNat x * fromNat y)

factorial :: Integer -> Integer
factorial n = product [1 .. n]

--- >>> fac (Suc (Suc (Suc Zero)))
-- Suc (Suc (Suc (Suc (Suc (Suc Zero)))))
fac :: Nat -> Nat
fac x = toNat (factorial (fromNat x))

{-
(Node
    (Node
        (Leaf 2)
        (Node
            (Leaf 3)
            (Node
                (Leaf 4)
                (Leaf 4)
            )
        )
    )
    (Leaf 1)
)
-}

data Tree a = Leaf a | Node (Tree a) (Tree a) deriving (Show)

--- >>> height (Node (Node (Leaf 1) (Leaf 1)) (Node (Leaf 1) (Leaf 1)))
-- 2
height :: Tree a -> Int
height x = d x 0
  where
    d :: Tree x -> Int -> Int
    d (Node a b) h = if x > y then x else y
      where
        x = d a k
        y = d b k
        k = h + 1
    d (Leaf _) h = h

--- >>> size (Node (Node (Leaf 1) (Leaf 1)) (Node (Leaf 1) (Leaf 1)))
-- 7
size :: Tree a -> Int
size = d
  where
    d :: Tree x -> Int
    d (Node a b) = d a + d b + 1
    d (Leaf _) = 1

--- >>> avg (Node (Leaf 4) (Node (Leaf 7) (Leaf 7)))
-- 6
avg :: Tree Int -> Int
avg t =
  let (c, s) = go t
   in s `div` c
  where
    go :: Tree Int -> (Int, Int)
    go tree = (count tree, sum tree)
      where
        sum :: Tree Int -> Int
        sum (Node a b) = sum a + sum b
        sum (Leaf c) = c

        count :: Tree x -> Int
        count (Node a b) = count a + count b
        count (Leaf _) = 1

{-
    1+2+3 * 4+5 = 1*4 + 1*5 + 2*4 + 2*5 + 3*4 + 3*5 = 54

    > e (1 + 2) * 3
    > (e 1 * e 3) + (e 2 * e 3)

    > e (((1 + 2) + 3) * (4 + 5))
    > e ( ((1+2) * (4+5)) + (3 * (4+5)) )

    > 1*3 + 1*4 + 2*3 + 2*4
    > e (1+2) * (3+4)
    > (1 * 3+4) + (2 * 3+4)
-}

infixl 6 :+:

infixl 7 :*:

data Expr = Val Int | Expr :+: Expr | Expr :*: Expr
  deriving (Show, Eq)

--- >>> evaluate (((Val 1 :*: Val 4 :+: Val 1 :*: Val 5) :+: (Val 2 :*: Val 4 :+: Val 2 :*: Val 5)) :+: (Val 3 :*: Val 4 :+: Val 3 :*: Val 5))
-- 54
evaluate :: Expr -> Int
evaluate (e1 :+: e2) = evaluate e1 + evaluate e2
evaluate (e1 :*: e2) = evaluate e1 * evaluate e2
evaluate (Val x) = x

--- >>> expand ((Val 1 :+: Val 2 :+: Val 3) :*: (Val 4 :+: Val 5))
-- (Val 1 :+: Val 2) :*: (Val 4 :+: Val 5) :+: Val 3 :*: (Val 4 :+: Val 5)
ex ((e2 :+: e3) :*: e1) = (ex e2 :*: ex e1) :+: (ex e3 :*: ex e1)
ex (e1 :*: (e2 :+: e3)) = (ex e1 :*: ex e2) :+: (ex e1 :*: ex e3)
ex (e1 :+: e2) = ex e1 :+: ex e2
ex (e1 :*: e2) = ex e1 :*: ex e2
ex e = e

--- >>> ex ((Val 1 :+: Val 2 :+: Val 3) :*: (Val 4 :+: Val 5))
-- ((Val 1 :*: Val 4 :+: Val 1 :*: Val 5) :+: (Val 2 :*: Val 4 :+: Val 2 :*: Val 5)) :+: (Val 3 :*: Val 4 :+: Val 3 :*: Val 5)
expand :: Expr -> Expr
expand x = if a == b then a else expand b
  where
    a = ex x
    b = ex . ex $ x

-- type Endo a = a -> a
-- Endo (Endo Int) = (Endo Int) -> (Endo Int) = (Int -> Int) -> (Int -> Int)

newtype Xor = Xor {getXor :: Bool}
  deriving (Eq, Show)

instance Semigroup Xor where
  (Xor a) <> (Xor b) = Xor (a `xor` b)

instance Monoid Xor where
  mempty = Xor False
  mappend = (Data.Semigroup.<>)

newtype Maybe' a = Maybe' {getMaybe :: Maybe a}
  deriving (Eq, Show)

instance Monoid a => Semigroup (Maybe' a) where
  a <> b = b

instance Monoid a => Monoid (Maybe' a) where
  mempty = Maybe' Nothing
  mappend = (Data.Semigroup.<>)