module Module54 where

import Data.Char (isDigit)

data Token = Number Int | Plus | Minus | LeftBrace | RightBrace
  deriving (Eq, Show)

asToken :: String -> Maybe Token
asToken "(" = Just LeftBrace
asToken ")" = Just RightBrace
asToken "+" = Just Plus
asToken "-" = Just Minus
asToken x = if all isDigit x then Just $ Number $ read x else Nothing

tokenize :: String -> Maybe [Token]
tokenize = mapM asToken . words

{-  -}

data Board = Position [[Int]] deriving (Show)

nextPositions :: [[Int]] -> [[[Int]]]
nextPositions x = [[t : xs] | xs <- x, t <- [1, 2, 3]]

nextPositionsN :: [[Int]] -> Int -> ([[Int]] -> Bool) -> [[[Int]]]
nextPositionsN _ x _ | x < 0 = []
nextPositionsN b n p = filter p (tail $ k [b] (n + 1))

k _ 0 = []
k b n = b ++ k (concatMap nextPositions b) (n - 1)

--- >>> k [[[1]]] 2
-- [[[1]],[[1,1]],[[2,1]],[[3,1]]]

nextPositionsN' :: Board -> Int -> (Board -> Bool) -> [Board]
nextPositionsN' _ x _ | x < 0 = []
nextPositionsN' b n pred = undefined

-- >>> nextPositionsN [[1]] (-1) (\x ->  (sum . (fmap sum)) x < 7)
-- []

-- >>> nextPositionsN [[1]] 0 (\x ->  (sum . (fmap sum)) x < 7)
-- [[1]]

-- >>> nextPositionsN [[1]] 1 (\x ->  (sum . (fmap sum)) x < 7)
-- [[[1,1]],[[2,1]],[[3,1]]]

-- >>> nextPositionsN [[1]] 2 (\x ->  (sum . (fmap sum)) x < 7)
-- [[[1,1,1]],[[2,1,1]],[[3,1,1]],[[1,2,1]],[[2,2,1]],[[3,2,1]],[[1,3,1]],[[2,3,1]]]

{-  -}

pythagoreanTriple :: Int -> [(Int, Int, Int)]
pythagoreanTriple x | x <= 0 = []
pythagoreanTriple x = do
  a <- [0 .. x]
  True <- return (a > 0)
  b <- [0 .. x]
  True <- return (b > 0)
  True <- return (a < b)
  c <- [0 .. x]
  True <- return (c > 0)
  True <- return (c <= x)
  True <- return ((a ^ 2) + (b ^ 2) == (c ^ 2))
  return (a, b, c)

-- >>> pythagoreanTriple 10
-- [(3,4,5),(6,8,10)]
