module Module53 where

data SomeType a = Random a

instance Functor SomeType where
  fmap f x = x >>= (\g -> return (f g))
