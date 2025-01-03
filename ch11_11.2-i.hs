{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}

import Data.Functor.Identity (Identity (Identity))
import Data.Kind (Type)
import Data.Proxy (Proxy (Proxy))
import Fcf (Eval, FindIndex, FromMaybe, Stuck, TyEq, type (=<<))
import GHC.TypeLits (KnownNat, natVal)
import Unsafe.Coerce (unsafeCoerce)

data OpenSum (f :: k -> Type) (ts :: [k]) where
  UnsafeOpenSum :: Int -> f t -> OpenSum f ts

type FindElem (key :: k) (ts :: [k]) =
  FromMaybe Stuck =<< FindIndex (TyEq key) ts

type Member t ts =
  KnownNat (Eval (FindElem t ts))

findElem :: forall t ts. Member t ts => Int
findElem = fromIntegral $ natVal $ Proxy @(Eval (FindElem t ts))

inject :: forall f t ts. Member t ts => f t -> OpenSum f ts
inject = UnsafeOpenSum $ findElem @t @ts

project :: forall f t ts. Member t ts => OpenSum f ts -> Maybe (f t)
project (UnsafeOpenSum i f) =
  if i == findElem @t @ts
    then Just $ unsafeCoerce f
    else Nothing

decompose :: OpenSum f (t ': ts) -> Either (f t) (OpenSum f ts)
decompose (UnsafeOpenSum 0 t) = Left $ unsafeCoerce t
decompose (UnsafeOpenSum n t) = Right $ UnsafeOpenSum (n - 1) t

-- Exercise 11.2-i
-- Write `weaken :: OpenSum f ts -> OpenSum f (x ': ts)`.
weaken :: OpenSum f ts -> OpenSum f (x ': ts)
weaken (UnsafeOpenSum n t) = UnsafeOpenSum (n + 1) t

-- Example usage:
type SumTypes = '[Bool, Int]

osum1 :: OpenSum Identity SumTypes
osum1 = inject (Identity True)

projVal :: Maybe (Identity Bool)
projVal = project osum1
