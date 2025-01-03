import Data.Kind

data HList (ts :: [Type]) where
  HNil :: HList '[]
  (:#) :: t -> HList ts -> HList (t ': ts)

infixr 5 :#

instance Eq (HList '[]) where
  HNil == HNil = True

instance (Eq t, Eq (HList ts)) => Eq (HList (t ': ts)) where
  (a :# as) == (b :# bs) = a == b && as == bs

-- Exercise 5.3-i
-- Implement `Ord` for `HList`:
instance Ord (HList '[]) where
  compare HNil HNil = EQ

-- I always forget `Ord` is a monoid. This version is less elegant, but works
-- just as well as Sandy's:
instance (Ord t, Ord (HList ts)) => Ord (HList (t ': ts)) where
  compare (a :# as) (b :# bs) =
    case compare a b of
      EQ -> compare as bs
      other -> other

-- Exercise 5.3-ii
-- Implement `Show` for `HList`:
instance Show (HList '[]) where
  show HNil = "HNil"

instance (Show t, Show (HList ts)) => Show (HList (t ': ts)) where
  show (a :# as) = show a <> " :# " <> show as
