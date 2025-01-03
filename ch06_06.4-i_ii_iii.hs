newtype Cont a = Cont
  { unCont :: forall r. (a -> r) -> r
  }

-- I could not figure out `Functor`. After peeking Sandy's solution I was able
-- to figure out `Applicative`, but failed miserably with `Monad`. I added
-- annotations on the types of all the terms on my attempt to figure it out.

-- Exercise 6.4-i
-- Provide a `Functor` instance for `Cont`.
instance Functor Cont where
  -- fmap      :: (a -> b) -> Cont a -> Cont b
  -- f         :: a -> b
  -- a         :: (a -> r) -> r
  -- c         :: b -> r
  -- c . f     :: a -> r
  -- a $ c . f :: r
  fmap f (Cont a) = Cont $ \c -> a $ c . f

-- Exercise 6.4-ii
-- Provide a `Applicative` instances for `Cont`.
instance Applicative Cont where
  pure a = Cont $ \c -> c a

  -- (<*>) :: Cont (a -> b) -> Cont a -> Cont b
  -- f     :: ((a -> b) -> r) -> r
  -- a     :: (a -> r) -> r
  -- c     :: b -> r
  -- c'    :: a -> b
  Cont f <*> Cont a = Cont $ \c -> f $ \c' -> a $ c . c'

-- Exercise 6.4-iii
-- Provide a `Monad` instances for `Cont`.
instance Monad Cont where
  -- (>>=)      :: Cont a -> (a -> Cont b) -> Cont b
  -- a          :: (a -> r) -> r
  -- b          :: (b -> r) -> r
  -- f          :: a -> Cont b
  -- unCont . f :: a -> (b -> r) -> r
  -- c          :: b -> r
  -- c'         :: a
  Cont a >>= f = Cont $ \c -> a $ \c' -> unCont (f c') c
