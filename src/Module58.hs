module Module58 where

import Control.Monad.State (State, execState, modify, replicateM)
import Data.Char (intToDigit)

fibStep :: State (Integer, Integer) ()
fibStep = modify (\(a, b) -> (b, a + b))

-- >>> execState fibStep (1,2)
-- (2,3)

fib :: Int -> Integer
fib n = fst $ execStateN n fibStep (0, 1)

-- >>> fib 6
-- 8

execStateN :: Int -> State s a -> s -> s
execStateN n m = execState $ replicateM n m

{-  -}

data Tree a = Leaf a | Fork (Tree a) a (Tree a) deriving (Show)

-- >>> numberTree (Leaf ())
-- Leaf 1

-- >>> numberTree (Fork (Leaf ()) () (Leaf ()))
-- Fork (Leaf 1) 2 (Leaf 3)

numberTree :: Tree () -> Tree Integer
numberTree tree = fst $ k tree 1
  where
    k :: Tree () -> Integer -> (Tree Integer, Integer)
    k (Leaf _) n = (Leaf n, n + 1)
    k (Fork a b c) n =
      let (uTree, u) = k a n
          (jTree, j) = k c (u + 1)
       in (Fork uTree u jTree, j)

s = 240 `div` 10

-- >>> s
-- 24

solve n d k = undefined
