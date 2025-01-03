{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

import Data.Functor.Identity (Identity (Identity))
import Data.Kind (Type)
import Data.Proxy (Proxy (Proxy))
import Fcf (Eval, FindIndex, FromMaybe, Stuck, TyEq, type (=<<))
import GHC.TypeLits (ErrorMessage (ShowType, Text, (:$$:), (:<>:)), KnownNat, TypeError, natVal)
import Unsafe.Coerce (unsafeCoerce)

data OpenSum (f :: k -> Type) (ts :: [k]) where
  UnsafeOpenSum :: Int -> f t -> OpenSum f ts

type FindElem (key :: k) (ts :: [k]) =
  FromMaybe Stuck =<< FindIndex (TyEq key) ts

-- Exercise 12-ii
-- Write a closed type family of kind `[k] -> ErrorMessage` that pretty prints
-- a list. Use it to improve the error message from `FriendlyFindElem`.

-- Here Sandy gives an equation for the intermediate pattern `(t ': '[])`. I
-- don't. Even so, recursion seems to work just fine up to the base case of
-- the empty list.
type family PrettyList (ts :: [k]) where
  PrettyList '[] = 'Text ""
  PrettyList (t ': ts) = 'Text " :: " ':<>: 'ShowType t ':$$: PrettyList ts

type family FriendlyFindElem (f :: k -> Type) (t :: k) (ts :: [k]) where
  FriendlyFindElem f t ts =
    FromMaybe
      ( TypeError
          ( 'Text "Attempted to call 'friendlyProject' to produce a `"
              ':<>: 'ShowType (f t)
              ':<>: 'Text "'."
              ':$$: 'Text "But the OpenSum can only contain one of: "
              ':$$: PrettyList ts
          )
      )
      =<< FindIndex (TyEq t) ts

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

friendlyProject
  :: forall f t ts
   . ( KnownNat (Eval (FriendlyFindElem f t ts))
     , Member t ts
     )
  => OpenSum f ts
  -> Maybe (f t)
friendlyProject (UnsafeOpenSum i f) =
  if i == findElem @t @ts
    then Just $ unsafeCoerce f
    else Nothing

decompose :: OpenSum f (t ': ts) -> Either (f t) (OpenSum f ts)
decompose (UnsafeOpenSum 0 t) = Left $ unsafeCoerce t
decompose (UnsafeOpenSum n t) = Right $ UnsafeOpenSum (n - 1) t

weaken :: OpenSum f ts -> OpenSum f (x ': ts)
weaken (UnsafeOpenSum n t) = UnsafeOpenSum (n + 1) t

-- usage example
type SumTypes = '[Bool, Int]

osum1 :: OpenSum Identity SumTypes
osum1 = inject (Identity True)

projVal :: Maybe (Identity Bool)
projVal = project osum1
