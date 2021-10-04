module Module52 where

import Control.Monad (ap, liftM)

-- Введём следующий тип:

data Log a = Log [String] a deriving (Show)

-- Реализуйте вычисление с логированием, используя Log. Для начала определите функцию toLogger

-- toLogger :: (a -> b) -> String -> (a -> Log b)
-- которая превращает обычную функцию, в функцию с логированием:

add1Log = toLogger (+ 1) "added one"

mult2Log = toLogger (* 2) "multiplied by 2"

-- >>> (toLogger (+1) "added one") 3
-- Log ["added one"] 4
-- >>> (toLogger (* 2) "multiplied by 2") 3
-- Log ["multiplied by 2"] 6

-- execLoggers :: a -> (a -> Log b) -> (b -> Log c) -> Log c
-- Которая принимает некоторый элемент и две функции с логированием. execLoggers возвращает результат последовательного применения функций к элементу и список сообщений, которые были выданы при применении каждой из функций:
-- >>> execLoggers 3 (toLogger (+1) "added one") (toLogger (* 2) "multiplied by 2")
-- Log ["added one","multiplied by 2"] 8

toLogger :: (a -> b) -> String -> (a -> Log b)
toLogger f msg x = Log [msg] (f x)

execLoggers :: a -> (a -> Log b) -> (b -> Log c) -> Log c
execLoggers x f g = Log (f1 ++ f2) d2
  where
    Log f1 d1 = f x
    Log f2 d2 = g d1

-- Функции с логированием из предыдущего задания возвращают в качестве результата значение с некоторой дополнительной
-- информацией в виде списка сообщений. Этот список является контекстом. Реализуйте функцию returnLog

-- returnLog :: a -> Log a

-- которая является аналогом функции return для контекста Log. Данная функция должна возвращать переданное ей значение с пустым контекстом.

returnLog :: a -> Log a
returnLog = Log []

-- Реализуйте фукцию bindLog

-- bindLog :: Log a -> (a -> Log b) -> Log b
-- которая работает подобно оператору >>= для контекста Log.

-- >>> Log ["nothing done yet"] 0 `bindLog` add1Log
-- Log ["nothing done yet","added one"] 1

-- >>> Log ["nothing done yet"] 3 `bindLog` add1Log `bindLog` mult2Log
-- Log ["nothing done yet","added one","multiplied by 2"] 8

bindLog :: Log a -> (a -> Log b) -> Log b
bindLog a b = Log (k ++ m) n
  where
    Log k l = a
    Log m n = b l

-- Реализованные ранее returnLog и bindLog позволяют объявить тип Log представителем класса Monad:

instance Functor Log where
  fmap = liftM

instance Applicative Log where
  pure = return
  (<*>) = ap

instance Monad Log where
  return = returnLog
  (>>=) = bindLog

-- Используя return и >>=, определите функцию execLoggersList

-- execLoggersList :: a -> [a -> Log a] -> Log a
-- которая принимает некоторый элемент, список функций с логированием и возвращает результат
-- последовательного применения всех функций в списке к переданному элементу вместе со списком сообщений,
-- которые возвращались данными функциями:

-- >>> execLoggersList 3 [add1Log, mult2Log, \x -> Log ["multiplied by 100"] (x * 100)]
-- Log ["added one","multiplied by 2","multiplied by 100"] 800

execLoggersList :: a -> [a -> Log a] -> Log a
execLoggersList = foldl (>>=) . return
