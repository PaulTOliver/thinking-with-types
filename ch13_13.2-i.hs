{-# LANGUAGE ImportQualifiedPost #-}

-- Exercise 13.2-i
-- Provide a generic instance for the `Ord` class.

import GHC.Generics qualified as G

class GOrd a where
  gcompare :: a x -> a x -> Ordering

instance GOrd G.U1 where
  gcompare G.U1 G.U1 = EQ

instance GOrd G.V1 where
  gcompare _ _ = EQ

instance Ord a => GOrd (G.K1 _1 a) where
  gcompare (G.K1 a) (G.K1 b) = compare a b

instance (GOrd a, GOrd b) => GOrd (a G.:+: b) where
  gcompare (G.L1 a1) (G.L1 a2) = gcompare a1 a2
  gcompare (G.R1 b1) (G.R1 b2) = gcompare b1 b2
  gcompare (G.L1 _) (G.R1 _) = LT
  gcompare (G.R1 _) (G.L1 _) = GT

instance (GOrd a, GOrd b) => GOrd (a G.:*: b) where
  -- Don't forget `Ord` is a monoid! :)
  gcompare (a1 G.:*: b1) (a2 G.:*: b2) = gcompare a1 a2 <> gcompare b1 b2

instance GOrd a => GOrd (G.M1 _x _y a) where
  gcompare (G.M1 a1) (G.M1 a2) = gcompare a1 a2

genericCompare :: (G.Generic a, GOrd (G.Rep a)) => a -> a -> Ordering
genericCompare a b = gcompare (G.from a) (G.from b)

-- Example usage:
data Foo a b c = F0 | F1 a | F2 b c deriving (Eq, G.Generic)

instance (Ord a, Ord b, Ord c) => Ord (Foo a b c) where
  compare = genericCompare
