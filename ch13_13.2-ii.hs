{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ImportQualifiedPost #-}

-- Exercise 13.2-ii
-- Use `GHC.Generics` to implement the function `exNihilo :: Maybe a`.

import GHC.Generics qualified as G

class GExNihilo a where
  gexNihilo :: Maybe (a x)

instance GExNihilo G.U1 where
  gexNihilo = Just G.U1

instance GExNihilo G.V1 where
  gexNihilo = Nothing

instance GExNihilo (G.K1 _1 a) where
  gexNihilo = Nothing

instance GExNihilo (a G.:+: b) where
  gexNihilo = Nothing

instance GExNihilo (a G.:*: b) where
  gexNihilo = Nothing

instance GExNihilo a => GExNihilo (G.M1 _x _y a) where
  gexNihilo = G.M1 <$> gexNihilo

-- I could not figure this one out so had to look at the solution. I noticed
-- Sandy missed to provide the actual implementation of `exNihilo` so here is
-- my version:
exNihilo :: forall a. (G.Generic a, GExNihilo (G.Rep a)) => Maybe a
exNihilo = G.to <$> gexNihilo @(G.Rep a)

-- Example usage:
data MyUnit = MyUnit deriving (G.Generic, Show)

newtype MyType a = MyType a deriving (G.Generic, Show)

data MySum = MySum1 | MySum2 deriving (G.Generic, Show)

t1 = exNihilo @MyUnit -- Just MyUnit

t2 = exNihilo @(MyType ()) -- Nothing

t3 = exNihilo @MySum -- Nothing
