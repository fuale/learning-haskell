module Lib where

import Control.Exception
import Data.Char (isDigit, isUpper)
import Data.Function (on)
import Data.List
import System.IO

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
      trapezoid x y = (f y + f x) / 2 * divisions

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
      trapezoid x = (f (x + divisions) + f x) / 2 * divisions
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
sumFstFst'' = (+) `on` fst . fst

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
    | otherwise = undefined

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

-- | 3.1
-- >>> oddsOnly' [1,2,3,4,5,6,7]
-- [7,5,3,1]
oddsOnly' :: Integral a => [a] -> [a] -- Почему наоборот?
oddsOnly' xs =
  let cc (x : xs) r = if odd x then cc xs (x : r) else cc xs r
      cc [] r = r
   in cc xs []

-- >>> oddsOnly [1,2,3,4,5,6,7]
oddsOnly :: Integral a => [a] -> [a]
oddsOnly xs =
  let cc (x : xs) r = if odd x then cc xs (x : r) else cc xs r
      cc [] r = r
   in reverse $ cc xs []

-- | 3.1
isPalindrome :: Eq a => [a] -> Bool
isPalindrome x = x == reverse x

-- | 3.1 Составьте список сумм соответствующих элементов трех заданных списков.
-- Длина результирующего списка должна быть равна длине самого длинного из
-- заданных списков, при этом «закончившиеся» списки не должны давать вклада в суммы.
-- >>> sum3 [1,1,1] [1,1,1] [2,2,2,2]
-- [4,4,4,2]
sum3 :: Num a => [a] -> [a] -> [a] -> [a]
sum3 (x : xs) (y : ys) (z : zs) = (x + y + z) : sum3 xs ys zs
sum3 (x : xs) (y : ys) _ = (x + y) : sum3 xs ys []
sum3 (x : xs) _ (z : zs) = (x + z) : sum3 xs [] zs
sum3 _ (y : ys) (z : zs) = (y + z) : sum3 [] ys zs
sum3 (x : xs) _ _ = x : sum3 xs [] []
sum3 _ (y : ys) _ = y : sum3 [] ys []
sum3 _ _ (z : zs) = z : sum3 [] [] zs
sum3 _ _ _ = []

-- | 3.1 Напишите функцию groupElems которая группирует одинаковые элементы в
-- списке (если они идут подряд) и возвращает список таких групп.
-- >>> groupElems [1,1,2,2,3,2,4,5,4,4,4,4]
-- [[1,1],[2,2],[3],[2],[4],[5],[4,4,4,4]]
groupElems :: Eq a => [a] -> [[a]]
groupElems [] = []
groupElems xs =
  let (a, b) = span (== head xs) xs
   in a : groupElems b

groupElems' :: Eq a => [a] -> [[a]]
groupElems' xs =
  let f [] [] m = m
      f (x : xs) [] m = f xs [x] m
      f [] ys m = ys : m
      f (x : xs) ys m =
        if x == head ys
          then f xs (x : ys) m
          else f xs [] (ys : m)
   in reverse $ f xs [] []

-- | 3.2 Напишите функцию readDigits, принимающую строку и возвращающую пару строк.
-- Первый элемент пары содержит цифровой префикс исходной строки, а второй - ее оставшуюся часть.
readDigits :: String -> (String, String)
readDigits = span isDigit

-- | 3.2 Реализуйте функцию filterDisj, принимающую два унарных предиката и список, и возвращающую
-- список элементов, удовлетворяющих хотя бы одному из предикатов.
filterDisj :: (a -> Bool) -> (a -> Bool) -> [a] -> [a]
filterDisj p1 p2 = filter (\x -> p1 x || p2 x)

-- | 3.2 Напишите реализацию функции qsort. Функция qsort должная принимать на вход список элементов
-- и сортировать его в порядке возрастания с помощью сортировки Хоара: для какого-то элемента x
-- изначального списка (обычно выбирают первый) делить список на элементы меньше и не меньше x,
-- и потом запускаться рекурсивно на обеих частях.
-- >>> qsort [3,4,3,2,1]
-- [1,2,3,3,4]
qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x : xs) =
  let la = filter (<= x) xs
      lr = filter (> x) xs
   in qsort la ++ [x] ++ qsort lr

-- | 3.2 Напишите функцию squares'n'cubes, принимающую список чисел,
-- и возвращающую список квадратов и кубов элементов исходного списка.
-- >>> squares'n'cubes [1,2,3,4]
-- [1,1,4,8,9,27,16,64]
squares'n'cubes :: Num a => [a] -> [a]
squares'n'cubes = concatMap (\x -> [x ^ 2, x ^ 3])

-- | 3.2 Воспользовавшись функциями map и concatMap, определите функцию perms,
-- которая возвращает все перестановки, которые можно получить из данного списка, в любом порядке.
-- Считайте, что все элементы в списке уникальны, и что для пустого списка имеется одна перестановка.
-- >>> perms [1,2,3]
-- [[1,2,3],[1,3,2],[2,3,1],[2,1,3],[3,2,1],[3,1,2]]
perms :: [a] -> [[a]]
perms xs = k xs []
  where
    k [] _ = [[]]
    k [y] ys = map (y :) (k ys [])
    k (y : ys) zs = k [y] (ys ++ zs) ++ k ys (y : zs)

-- | Реализуйте функцию delAllUpper, удаляющую из текста все слова, целиком состоящие из символов в верхнем регистре.
-- Предполагается, что текст состоит только из символов алфавита и пробелов, знаки пунктуации, цифры и т.п. отсутствуют.
delAllUpper :: String -> String
delAllUpper = unwords . filter (not . all isUpper) . words

-- | Напишите функцию max3, которой передаются три списка одинаковой длины и которая возвращает список той же длины,
-- содержащий на k-ой позиции наибольшее значение из величин на этой позиции в списках-аргументах.
-- >>> max3 [7,2,9] [3,6,8] [1,8,10]
max3 :: Ord a => [a] -> [a] -> [a] -> [a]
max3 = zipWith3 (\x y z -> maximum [x, y, z])

-- | Реализуйте c использованием функции zipWith функцию fibStream, возвращающую бесконечный список чисел Фибоначчи.
-- >>> take 10 fibStream
-- [0,1,1,2,3,5,8,13,21,34]
fibStream :: [Integer]
fibStream = 0 : 1 : zipWith (+) fibStream (tail fibStream)

-- | Предположим, что функция repeat, была бы определена следующим образом:
repeat :: a -> [a]
repeat = iterate repeatHelper

repeatHelper :: a -> a
repeatHelper = id

-- | Пусть задан тип Odd нечетных чисел следующим образом:
-- Сделайте этот тип представителем класса типов Enum
newtype Odd = Odd Integer
  deriving (Eq, Show)

instance Enum Odd where
  toEnum x
    | odd x = Odd $ toInteger x
    | otherwise = error "oops!"
  fromEnum (Odd x) = fromInteger x
  succ (Odd x) = Odd (x + 2)
  pred (Odd x) = Odd (x - 2)
  enumFrom x = x : enumFrom (succ x)
  enumFromTo (Odd x) (Odd y)
    | x <= y = takeWhile (/= (succ $ Odd y)) $ enumFrom (Odd x)
    | x > y = []
    | otherwise = undefined
    where
      enumDownFrom x = x : enumDownFrom (pred x)
  enumFromThen (Odd x) (Odd y) = Odd x : enumFromThen (Odd y) (Odd $ y + y - x)
  enumFromThenTo (Odd x) (Odd y) (Odd z)
    | x < z && x > y = []
    | x > z && x < y = []
    | x > z && x == y = []
    | x < z = takeWhile (\(Odd x) -> x <= z) $ enumFromThen (Odd x) (Odd y)
    | x >= z = takeWhile (\(Odd x) -> x >= z) $ enumFromThen (Odd x) (Odd y)
    | otherwise = undefined

-- | Пусть есть список положительных достоинств монет coins, отсортированный по возрастанию.
-- Воспользовавшись механизмом генераторов списков, напишите функцию change,
-- которая разбивает переданную ей положительную сумму денег на монеты достоинств
-- из списка coins всеми возможными способами.
coins :: [Integer]
coins = [2, 3, 7]

-- >>> change' 7
-- [[3,2,2],[2,3,2],[2,2,3],[7]]
change' :: Integer -> [[Integer]]
change' sum = h sum []
  where
    h sum last
      | sum < 0 = []
      | sum == 0 = [last]
      | otherwise = concatMap (\coin -> h (sum - coin) (coin : last)) coins

-- | Напишите реализацию функции concatList через foldr
{-# ANN concatList "HLint: ignore Use concat" #-} -- ignore
concatList :: [[a]] -> [a]
concatList = foldr (++) []

-- | Используя функцию foldr, напишите реализацию функции lengthList, вычисляющей количество элементов в списке.
-- >>> lengthList [1,2,3,4,5]
-- 5
lengthList :: [a] -> Int
lengthList = foldr (\x s -> s + 1) 0

-- | Реализуйте функцию sumOdd, которая суммирует элементы списка целых чисел, имеющие нечетные значения:
sumOdd :: [Integer] -> Integer
sumOdd = foldr (\x s -> if odd x then x + s else s) 0

-- | Реализуйте функцию meanList, которая находит среднее значение
-- элементов списка, используя однократный вызов функции свертки.
-- >>> meanList [2,2,8,8]
-- 5.0
meanList :: [Double] -> Double
meanList = (\(x, y) -> y / x) . foldr (\x (s, m) -> (s + 1, x + m)) (0, 0)

-- | Используя однократный вызов свертки, реализуйте функцию evenOnly, которая
-- выбрасывает из списка элементы, стоящие на нечетных местах, оставляя только четные.
evenOnly' :: [a] -> [a]
evenOnly' = reverse . snd . foldl (\(p, e) x -> f x (p, e)) (1, [])
  where
    f x (p, e)
      | even p = (p + 1, x : e)
      | otherwise = (p + 1, e)

-- :D
-- >>> evenOnly [1..10]
-- [2,4,6,8,10]
evenOnly :: [a] -> [a]
evenOnly (_ : a : as) = a : evenOnly as
evenOnly _ = []

-- | Напишите реализацию функции, возвращающей последний элемент списка, через foldl1.
-- >>> lastElem [1,2,3,4,5]
lastElem :: [a] -> a
lastElem = foldl1 (const id)

-- | Используя unfoldr, реализуйте функцию, которая возвращает в обратном алфавитном порядке список символов, попадающих в
-- заданный парой диапазон. Попадание символа x в диапазон пары (a,b) означает, что x >= a и x <= b.
-- >>> revRange ('a', 'g')
revRange :: (Char, Char) -> [Char]
revRange = unfoldr g
  where
    g (x, y) = if y >= x then Just (y, (x, pred y)) else Nothing
