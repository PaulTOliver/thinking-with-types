{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE ImportQualifiedPost #-}

import Data.Kind (Type)
import Data.Proxy (Proxy (Proxy))
import Data.Vector (Vector, cons, empty, ifilter, unsafeIndex, (//))
import Fcf qualified as F
import Fcf.Data.List qualified as F (Cons)
import Fcf.Data.Nat qualified as F (Nat)
import GHC.OverloadedLabels (IsLabel, fromLabel)
import GHC.TypeLits (KnownNat, Symbol, natVal)
import Unsafe.Coerce (unsafeCoerce)

data Any (f :: k -> Type) where
  Any :: f t -> Any f

data OpenProduct (f :: k -> Type) (ts :: [(Symbol, k)]) where
  OpenProduct :: Vector (Any f) -> OpenProduct f ts

type UniqueKey (key :: k) (ts :: [(k, t)]) =
  F.Null F.=<< F.Filter (F.TyEq key F.<=< F.Fst) ts

type FindIndex (key :: Symbol) (ts :: [(Symbol, k)]) =
  F.FindIndex (F.TyEq key F.<=< F.Fst) ts

type FindElem (key :: Symbol) (ts :: [(Symbol, k)]) =
  F.Eval (F.FromMaybe F.Stuck F.=<< FindIndex key ts)

type LookupType (key :: k) (ts :: [(k, t)]) =
  F.FromMaybe F.Stuck F.=<< F.Lookup key ts

type UpdateElem (key :: Symbol) (t :: k) (ts :: [(Symbol, k)]) =
  F.SetIndex (FindElem key ts) '(key, t) ts

type DeleteElem (key :: Symbol) (ts :: [(Symbol, k)]) =
  F.Filter (F.Not F.<=< F.TyEq key F.<=< F.Fst) ts

type UpsertElem (key :: Symbol) (t :: k) (ts :: [(Symbol, k)]) =
  F.UnMaybe
    (F.Cons '(key, t) ts)
    (F.ConstFn (F.Eval (UpdateElem key t ts)))
    F.=<< FindIndex key ts

data Key (key :: Symbol) = Key

instance key ~ key' => IsLabel key (Key key') where
  fromLabel = Key

class MaybeIndex (m :: Maybe F.Nat) where
  maybeIndex :: Maybe Int

instance MaybeIndex 'Nothing where
  maybeIndex = Nothing

instance KnownNat a => MaybeIndex ('Just a) where
  maybeIndex = Just $ fromIntegral $ natVal $ Proxy @a

nil :: OpenProduct f '[]
nil = OpenProduct empty

insert
  :: F.Eval (UniqueKey key ts) ~ 'True
  => Key key
  -> f t
  -> OpenProduct f ts
  -> OpenProduct f ('(key, t) ': ts)
insert _ ft (OpenProduct v) = OpenProduct $ cons (Any ft) v

findElem
  :: forall key ts
   . KnownNat (FindElem key ts)
  => Int
findElem = fromIntegral $ natVal $ Proxy @(FindElem key ts)

get
  :: forall key ts f
   . KnownNat (FindElem key ts)
  => Key key
  -> OpenProduct f ts
  -> f (F.Eval (LookupType key ts))
get _ (OpenProduct v) = unAny $ unsafeIndex v $ findElem @key @ts
 where
  unAny (Any a) = unsafeCoerce a

update
  :: forall key ts t f
   . KnownNat (FindElem key ts)
  => Key key
  -> f t
  -> OpenProduct f ts
  -> OpenProduct f (F.Eval (UpdateElem key t ts))
update _ ft (OpenProduct v) = OpenProduct $ v // [(findElem @key @ts, Any ft)]

-- Exercise 11.3-i
-- Implement `delete` for `OpenProduct`s.

-- This one works identically to Sandy's version:
delete
  :: forall key ts f
   . KnownNat (FindElem key ts)
  => Key key
  -> OpenProduct f ts
  -> OpenProduct f (F.Eval (DeleteElem key ts))
delete _ (OpenProduct v) = OpenProduct $ flip ifilter v $ curry $ (findElem @key @ts ==) . fst

-- Exercise 11.3-ii
-- Implement `upsert` for `OpenProduct`s.

-- I came up with slightly different (but equivalent) helper data-types and
-- type families (see above).
upsert
  :: forall key ts t f
   . MaybeIndex (F.Eval (FindIndex key ts))
  => Key key
  -> f t
  -> OpenProduct f ts
  -> OpenProduct f (F.Eval (UpsertElem key t ts))
upsert _ ft (OpenProduct v) =
  case maybeIndex @(F.Eval (FindIndex key ts)) of
    Nothing -> OpenProduct $ cons (Any ft) v
    Just i -> OpenProduct $ v // [(i, Any ft)]
