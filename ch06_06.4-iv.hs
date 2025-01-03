-- Exercise 6.4-iv
-- There is also a monad transformer version of `Cont`. Implement it.
newtype ContT t a = ContT
  { unContT :: forall r. (a -> t r) -> t r
  }

instance Functor (ContT t) where
  fmap f (ContT a) = ContT $ \c -> a $ c . f

instance Applicative (ContT t) where
  pure a = ContT $ \c -> c a
  ContT f <*> ContT a = ContT $ \c -> f $ \c' -> a $ c . c'

instance Monad (ContT t) where
  ContT a >>= f = ContT $ \c -> a $ \c' -> unContT (f c') c
