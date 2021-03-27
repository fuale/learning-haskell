module Lib where

import Data.Function (on)

-- | Возвращает сумму и кол-во цифр числа
-- >>> sum'n'count 64
-- (10,2)
sum'n'count :: Integer -> (Integer, Integer)
sum'n'count x =
  let s = digitSumm (abs x) 0
      c = count (abs x) 0
   in (s, c)
  where
    count :: Integer -> Integer -> Integer
    count x c = if x > 10 then count (div x 10) (c + 1) else c + 1

    digitSumm :: Integer -> Integer -> Integer
    digitSumm x c = if x >= 10 then digitSumm (div x 10) (c + mod x 10) else c + x

-- | Интегрирование методом трапеций
-- >>> integration sin pi 0
-- -1.9999983550656846
integration :: (Double -> Double) -> Double -> Double -> Double
integration f a b =
  let divisions :: Double
      divisions = (b - a) / 1000

      trapezoid :: Double -> Double -> Double
      trapezoid x y = ((f y + f x) / 2) * divisions

      k :: Double -> Double -> Double -> Double
      k n point result =
        if n < 1000
          then k (n + 1) (point + divisions) (result + trapezoid point (point + divisions))
          else result
   in k 0 a 0

integrationList :: (Double -> Double) -> Double -> Double -> Double
integrationList f a b =
  let divisions :: Double
      divisions = (b - a) / 1000

      trapezoid :: Double -> Double
      trapezoid x = ((f (x + divisions) + f x) / 2) * divisions
   in sum $ map trapezoid [a, (a + divisions) .. b]

-- | Напишите функцию трех аргументов getSecondFrom, полиморфную по каждому из них,
--  которая полностью игнорирует первый и третий аргумент, а возвращает второй. Укажите ее тип.
-- >>> getSecondFrom 2 3 4
-- 3
getSecondFrom :: a -> b -> c -> b
getSecondFrom a b c = b

-- | Функция multSecond, перемножающая вторые элементы пар, реализована следующим образом
--  Напишите реализацию функций g и h.
-- >>> multSecond ('A', 10) ('B', 20)
-- 200
multSecond :: (a, Integer) -> (a, Integer) -> Integer
multSecond = g `on` h
  where
    g :: Integer -> Integer -> Integer
    g a b = b * a

    h :: (a, Integer) -> Integer
    h = snd

sumFstFst = (+) `on` helper
  where
    helper pp = fst $ fst pp

sumFstFst' :: ((Integer, b1), b2) -> ((Integer, b1), b2) -> Integer
sumFstFst' = (+) `on` \x -> fst $ fst x

sumFstFst'' :: ((Integer, b1), b2) -> ((Integer, b1), b2) -> Integer
sumFstFst'' = (+) `on` (fst . fst)

-- | Реализуйте функцию on3, имеющую семантику, схожую с on, но принимающую
--  в качестве первого аргумента трехместную функцию:
--  Например, сумма квадратов трех чисел может быть записана с использованием on3 так
-- >>> sum3squares 1 2 3
-- 14
on3 :: (b -> b -> b -> c) -> (a -> b) -> a -> a -> a -> c
on3 op f x y z = op (f x) (f y) (f z)

sum3squares :: Integer -> Integer -> Integer -> Integer
sum3squares = (\x y z -> x + y + z) `on3` (^ 2)

-- | Реализуйте функцию seqA, находящую элементы следующей рекуррентной последовательности
-- >>> seqA 301
seqA' :: Integer -> Integer
seqA' n
  | n == 0 = 1
  | n == 1 = 2
  | n == 2 = 3
  | n >= 3 = seqA' (n - 1) + seqA' (n - 2) - 2 * seqA' (n - 3)
  | otherwise = error "`n` must be more than or equal to 0"

seqA :: Integer -> Integer
seqA n
  | n == 0 = 1
  | n == 1 = 2
  | n == 2 = 3
  | otherwise = seqAAux (n - 2) 3 2 1
  where
    seqAAux :: Integer -> Integer -> Integer -> Integer -> Integer
    seqAAux n p1 p2 p3
      | n == 0 = p1
      | otherwise = seqAAux (n - 1) (p1 + p2 - 2 * p3) p1 p2

-- | Функция одной переменной doItYourself выбирает наибольшее из переданного ей аргумента и числа 42,
-- затем возводит результат выбора в куб и, наконец, вычисляет логарифм по основанию 2 от полученного числа. Эта функция реализована в виде:
-- doItYourself = f . g . h
-- Напишите реализации функций f, g и h. Постарайтесь сделать это в бесточечном стиле.
-- f = undefined
-- g = undefined
-- h = undefined
-- >>> doItYourself 12
-- 16.176952268336283
doItYourself :: Double -> Double
doItYourself = f . g . h
  where
    f :: Double -> Double
    f = logBase 2

    g :: Double -> Double
    g = (^ 3)

    h :: Double -> Double
    h = max 42

-- | 2.3
class Printable a where
  toString :: a -> String

instance Printable Bool where
  toString x
    | x = "true"
    | not x = "false"

instance Printable () where
  toString x = "unit type"

instance (Printable a, Printable b) => Printable (a, b) where
  toString (a, b) = "(" ++ toString a ++ "," ++ toString b ++ ")"

-- | 2.4
class KnownToGork a where
  stomp :: a -> a
  doesEnrageGork :: a -> Bool

class KnownToMork a where
  stab :: a -> a
  doesEnrageMork :: a -> Bool

class (KnownToGork a, KnownToMork a) => KnownToGorkAndMork a where
  stompOrStab :: a -> a
  stompOrStab x
    | doesEnrageMork x && doesEnrageGork x = stomp (stab x)
    | doesEnrageMork x = stomp x
    | doesEnrageGork x = stab x
    | otherwise = x

-- | 2.4
class (Enum a, Bounded a, Eq a) => SafeEnum a where
  ssucc :: a -> a
  ssucc a = if maxBound == a then minBound else succ a

  spred :: a -> a
  spred a = if minBound == a then maxBound else pred a

-- | 2.4
avg :: Int -> Int -> Int -> Double
avg a b c = fromInteger (toInteger a + toInteger b + toInteger c) / 3

-- | 3
addTwoElements :: a -> a -> [a] -> [a]
addTwoElements x y z = x : y : z

-- | 3.1
nTimes :: a -> Int -> [a]
nTimes a n =
  let k n x = if n > 0 then k (n - 1) (a : x) else x
   in k n []

nTimes' :: a -> Int -> [a]
nTimes' a n =
  let t = a : t
   in take n t
