-- Exercise 3-i
-- Which of these types are `Functor`s? Give instances for the ones that are.

-- `T1` is a `Functor`:
newtype T1 a
  = T1 (Int -> a)

instance Functor T1 where
  fmap f (T1 g) = T1 $ f <$> g

newtype T2 a
  = T2 (a -> Int)

newtype T3 a
  = T3 (a -> a)

newtype T4 a
  = T4 ((Int -> a) -> Int)

-- `T5` is a `Functor`:
newtype T5 a
  = T5 ((a -> Int) -> Int)

instance Functor T5 where
  fmap f (T5 g) = T5 $ \f' -> g $ f' . f
