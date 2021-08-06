module Lib where

import Data.Char (isDigit, isUpper)
import Data.Function (on, (&))
import Data.List (unfoldr)
import Data.Maybe (fromMaybe)
import Data.Time.Clock (UTCTime)
import Data.Time.Format (defaultTimeLocale, formatTime)

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

-- | Тип данных Color определен следующим образом
-- Определите экземпляр класса Show для типа Color, сопоставляющий каждому из трех цветов его текстовое представление.
data Color = Red | Green | Blue

--- >>> show Red
-- "Red"

instance Show Color where
  show Red = "Red"
  show Green = "Green"
  show Blue = "Blue"

charToInt :: Char -> Int
charToInt '0' = 0
charToInt '1' = 1
charToInt '2' = 2
charToInt '3' = 3
charToInt '4' = 4
charToInt '5' = 5
charToInt '6' = 6
charToInt '7' = 7
charToInt '8' = 8
charToInt '9' = 9
charToInt _ = undefined

-- | Определите (частичную) функцию stringToColor, которая по строковому представлению
-- цвета как в прошлой задаче возвращает исходный цвет.
stringToColor :: String -> Color
stringToColor "Red" = Red
stringToColor "Green" = Green
stringToColor "Blue" = Blue
stringToColor _ = undefined

-- | Тип LogLevel описывает различные уровни логирования.
data LogLevel = Error | Warning | Info

-- | Определите функцию cmp, сравнивающую элементы типа LogLevel так, чтобы было верно, что Error > Warning > Info.
-- >>> cmp Error Warning
-- GT
cmp :: LogLevel -> LogLevel -> Ordering
cmp Warning Warning = EQ
cmp Error Error = EQ
cmp Info Info = EQ
cmp Error _ = GT
cmp Warning Error = LT
cmp Warning Info = GT
cmp Info _ = LT

-- | Пусть объявлен следующий тип данных:
data Result = Fail | Success

data SomeData = Int

-- И допустим определен некоторый тип данных SomeData и некоторая функция
doSomeWork :: SomeData -> (Result, Int)
doSomeWork = undefined

-- возвращающая результат своей работы и либо код ошибки в случае неудачи, либо 0 в случае успеха.
-- Определите функцию processData, которая вызывает doSomeWork и возвращает строку "Success" в случае ее успешного завершения,
-- либо строку "Fail: N" в случае неудачи, где N — код ошибки.

processData :: SomeData -> String
processData f = case doSomeWork f of
  (Success, _) -> "Success"
  (_, x) -> "Fail: " ++ show x

data Point = Point Double Double

origin :: Point
origin = Point 0.0 0.0

distanceToOrigin :: Point -> Double
distanceToOrigin (Point x y) = sqrt (x ^ 2 + y ^ 2)

distance'old :: Point -> Point -> Double
distance'old (Point x1 y1) (Point x2 y2) = sqrt ((x2 - x1) ^ 2 + (y2 - y1) ^ 2)

-- | Определим тип фигур Shape:
data Shape = Circle Double | Rectangle Double Double

-- У него два конструктора: Circle r — окружность радиуса r,
-- и Rectangle a b — прямоугольник с размерами сторон a и b. Реализуйте функцию area,
-- возвращающую площадь фигуры. Константа pi уже определена в стандартной библиотеке.

area :: Shape -> Double
area (Circle r) = pi * r ^ 2
area (Rectangle a b) = a * b

-- | В одном из прошлых заданий мы встречали тип Result и функцию doSomeWork:
-- Функция doSomeWork возвращала результат своей работы и либо код ошибки в случае неудачи,
-- либо 0 в случае успеха. Такое определение функции не является наилучшим, так как в случае
-- успеха мы вынуждены возвращать некоторое значение, которое не несет никакой смысловой нагрузки.
-- Используя функцию doSomeWork, определите функцию doSomeWork' так, чтобы она возвращала код
-- ошибки только в случае неудачи. Для этого необходимо определить тип Result'. Кроме того, определите
-- instance Show для Result' так, чтобы show возвращал "Success" в случае успеха и "Fail: N" в случае
-- неудачи, где N — код ошибки.
data Result' = Fail' Int | Success'

instance Show Result' where
  show (Fail' m) = "Fail: " ++ show m
  show Success' = "Success"

doSomeWork' :: SomeData -> Result'
doSomeWork' m = case doSomeWork m of
  (Success, _) -> Success'
  (Fail, x) -> Fail' x

-- | Реализуйте функцию isSquare, проверяющую является ли фигура квадратом.
square :: Double -> Shape
square a = Rectangle a a

isSquare :: Shape -> Bool
isSquare (Rectangle x y)
  | x == y = True
  | otherwise = False
isSquare _ = False

-- | Целое число можно представить как список битов со знаком.
-- Реализуйте функции сложения и умножения для таких целых чисел, считая, что младшие биты идут
-- в начале списка, а старшие — в конце. Можно считать, что на вход не будут подаваться числа с ведущими нулями.
data Bit = Zero | One

data Sign = Minus | Plus

data Z = Z Sign [Bit]

instance Show Bit where
  show Zero = "0"
  show One = "1"

instance Show Z where
  show (Z Minus bit) = "-" ++ show bit
  show (Z Plus bit) = "+" ++ show bit

toNum :: [Bit] -> Int -> Int
toNum (One : xs) x = 1 * x + toNum xs x * 2
toNum (Zero : xs) x = toNum xs x * 2
toNum [] _ = 0

toBit :: Int -> [Bit]
toBit 0 = []
toBit x = (if odd x then One else Zero) : toBit (x `div` 2)

add :: Z -> Z -> Z
add (Z m1 s1) (Z m2 s2) = Z m s
  where
    helper a b = case m1 of
      Minus -> case m2 of
        Minus -> toNum a 1 + toNum b 1
        Plus -> toNum b 1 - toNum a 1
      Plus -> case m2 of
        Minus -> toNum a 1 - toNum b 1
        Plus -> toNum a 1 + toNum b 1

    x = helper s1 s2
    s = toBit $ abs x
    m = if x >= 0 then Plus else Minus

mul :: Z -> Z -> Z
mul (Z m1 s1) (Z m2 s2) = Z m (toBit $ abs x)
  where
    x = toNum s1 1 * toNum s2 1
    m = case m1 of
      Minus -> case m2 of
        Minus -> Plus
        Plus -> Minus
      Plus -> case m2 of
        Minus -> Minus
        Plus -> Plus

-- 14 * 10 = 140
-- 1110 * 1010 = 10001100df
-- >>> mul (Z Plus [Zero, One, One, One]) (Z Plus [Zero, One, Zero, One])
-- +[0,0,1,1,0,0,0,1]

-- | Определите тип записи, который хранит элементы лога. Имя конструктора должно
-- совпадать с именем типа, и запись должна содержать три поля:

-- timestamp — время, когда произошло событие (типа UTCTime);
-- logLevel — уровень события (типа LogLevel);
-- message — сообщение об ошибке (типа String).

-- Определите функцию logLevelToString, возвращающую текстуальное представление типа LogLevel,
-- и функцию logEntryToString, возвращающую текстуальное представление записи в виде:
-- <время>: <уровень>: <сообщение>
-- Для преобразование типа UTCTime в строку используйте функцию timeToString.
timeToString :: UTCTime -> String
timeToString = formatTime defaultTimeLocale "%a %d %T"

data LogEntry = LogEntry {timestamp :: UTCTime, logLevel :: LogLevel, message :: String}

logLevelToString :: LogLevel -> String
logLevelToString Info = "Info"
logLevelToString Error = "Error"
logLevelToString Warning = "Warning"

logEntryToString :: LogEntry -> String
logEntryToString e = timeToString (timestamp e) ++ ": " ++ logLevelToString (logLevel e) ++ ": " ++ message e

-- | Определите функцию updateLastName person1 person2, которая меняет фамилию person2 на фамилию person1.
data Person = Person {firstName :: String, lastName :: String, age :: Int}

updateLastName :: Person -> Person -> Person
updateLastName p1 p2 = p2 {lastName = lastName p1}

-- | Определить функцию abbrFirstName, которая сокращает имя до первой буквы с точкой, то есть, если имя было "Ivan", то после
-- применения этой функции оно превратится в "I.". Однако, если имя было короче двух символов, то оно не меняется.
abbrFirstName :: Person -> Person
abbrFirstName p = p {firstName = if length (firstName p) > 1 then head (firstName p) : "." else firstName p}

-- | Реализуйте функции distance, считающую расстояние между двумя точками с вещественными координатами,
-- и manhDistance, считающую манхэттенское расстояние между двумя точками с целочисленными координатами.
data Coord a = Coord a a

distance :: Coord Double -> Coord Double -> Double
distance (Coord x1 y1) (Coord x2 y2) = sqrt ((x2 - x1) ^ 2 + (y2 - y1) ^ 2)

manhDistance :: Coord Int -> Coord Int -> Int
manhDistance (Coord x1 y1) (Coord x2 y2) = abs (x1 - x2) + abs (y1 - y2)

-- Плоскость разбита на квадратные ячейки. Стороны ячеек параллельны осям координат.
-- Координаты углов ячейки с координатой (0,0) имеют неотрицательные координаты. Один из
-- углов этой ячейки имеет координату (0,0). С ростом координат ячеек увеличиваются координаты точек внутри этих ячеек.

-- Реализуйте функции getCenter, которая принимает координату ячейки и возвращает координату
-- ее центра, и функцию getCell, которая принимает координату точки и возвращает номер ячейки
--  в которой находится данная точка. В качестве первого аргумента обе эти функции принимают ширину ячейки

getCenter :: Double -> Coord Int -> Coord Double
getCenter w (Coord x1 y1) = Coord x2 y2
  where
    x2 = w * fromIntegral x1 + (w / 2)
    y2 = w * fromIntegral y1 + (w / 2)

getCell :: Double -> Coord Double -> Coord Int
getCell w (Coord x1 y1) = Coord x2 y2
  where
    x2 = floor (x1 / w)
    y2 = floor (y1 / w)

-- | Реализуйте функцию, которая ищет в строке первое вхождение символа, который является цифрой,
-- и возвращает Nothing, если в строке нет цифр.
findDigit :: [Char] -> Maybe Char
findDigit [] = Nothing
findDigit (x : xs) = if isDigit x then Just x else findDigit xs

findDigitOrX :: [Char] -> Char
findDigitOrX xs = fromMaybe 'X' (findDigit xs)

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just x) = [x]

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe xs = Just $ head xs