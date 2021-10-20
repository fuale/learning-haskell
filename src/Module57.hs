module Module57 where

import Control.Monad.Reader (Reader, ask, runReader)
import Control.Monad.State (State, runState, state)
import Control.Monad.Writer (Writer, execWriter, runWriter, tell)
import Data.Semigroup (Semigroup, Sum (Sum, getSum), (<>))

newtype Sum'n'List = Sum'n'List {getSum'n'List :: (Sum Integer, [String])}

instance Semigroup Sum'n'List where
  (<>) (Sum'n'List (n1, m1)) (Sum'n'List (n2, m2)) = Sum'n'List (n1 <> n2, m1 <> m2)

instance Monoid Sum'n'List where
  mempty = Sum'n'List (Sum 0, [""])
  mappend = (<>)

type Shopping = Writer Sum'n'List ()

shopping1 :: Shopping
shopping1 = do
  purchase "Jeans" 19200
  purchase "Water" 180
  purchase "Lettuce" 328

-- >>> total shopping1
-- 19708
-- >>> items shopping1
-- ["Jeans","Water","Lettuce"]
items :: Shopping -> [String]
items = snd . getSum'n'List . execWriter

purchase :: String -> Integer -> Shopping
purchase item cost = tell $ Sum'n'List (Sum cost, [item])

total :: Shopping -> Integer
total = getSum . fst . getSum'n'List . execWriter

{-  -}

-- >>> evalState (readerToState $ asks (+2)) 4
-- 6

-- >>> runState (readerToState $ asks (+2)) 4
-- (6,4)

readerToState :: Reader r a -> State r a
readerToState r = state (\x -> (runReader r x, x))

-- >>> runState (writerToState $ tell "world") "hello,"
-- ((),"hello,world")

-- >>> runState (writerToState $ tell "world") mempty
-- ((),"world")

writerToState :: Monoid w => Writer w a -> State w a
writerToState m = state s
  where
    s x = (fst o, x `mappend` snd o)
    o = runWriter m
