{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeFamilies #-}

import Data.Kind (Constraint, Type)
import Data.Proxy (Proxy (Proxy))
import Data.Vector (Vector, cons, empty, ifilter, unsafeIndex, (//))
import Fcf qualified as F
import Fcf.Data.List qualified as F (Cons)
import Fcf.Data.Nat qualified as F (Nat)
import GHC.OverloadedLabels (IsLabel, fromLabel)
import GHC.TypeLits qualified as T
import Unsafe.Coerce (unsafeCoerce)

data Any (f :: k -> Type) where
  Any :: f t -> Any f

data OpenProduct (f :: k -> Type) (ts :: [(T.Symbol, k)]) where
  OpenProduct :: Vector (Any f) -> OpenProduct f ts

type UniqueKey (key :: k) (ts :: [(k, t)]) =
  F.Null F.=<< F.Filter (F.TyEq key F.<=< F.Fst) ts

type family
  RequireUniqueKey
    (result :: Bool)
    (key :: T.Symbol)
    (t :: k)
    (ts :: [(T.Symbol, k)])
    :: Constraint
  where
  RequireUniqueKey 'True key t ts = ()
  RequireUniqueKey 'False key t ts =
    T.TypeError
      ( 'T.Text "Attempting to add a field named `"
          'T.:<>: 'T.Text key
          'T.:<>: 'T.Text "' with type "
          'T.:<>: 'T.ShowType t
          'T.:<>: 'T.Text " to an OpenProduct."
          'T.:$$: 'T.Text "But the OpenProduct already has a field `"
            'T.:<>: 'T.Text key
            'T.:<>: 'T.Text "' with type "
            'T.:<>: 'T.ShowType (F.Eval (LookupType key ts))
          'T.:$$: 'T.Text "Consider using `update' instead of `insert'."
      )

type FindIndex (key :: T.Symbol) (ts :: [(T.Symbol, k)]) =
  F.FindIndex (F.TyEq key F.<=< F.Fst) ts

-- Exercise 12-i
-- Add helpful type errors to `OpenProduct`'s `update` and `delete` functions.

-- My take is a bit different than Sandy's solution. `FindElemWithBase`
-- generalizes element finding, so we can re-implement the original `FindElem`
-- (which gets stuck) plus a new `FindElemDuring` (which provides the helpful
-- error message) using it.
type FindElemWithBase (key :: T.Symbol) (ts :: [(T.Symbol, k)]) (base :: T.Nat) =
  F.Eval (F.FromMaybe base F.=<< FindIndex key ts)

type FindElem (key :: T.Symbol) (ts :: [(T.Symbol, k)]) =
  FindElemWithBase key ts F.Stuck

type FindElemDuring (action :: T.Symbol) (key :: T.Symbol) (ts :: [(T.Symbol, k)]) =
  FindElemWithBase
    key
    ts
    ( T.TypeError
        ( 'T.Text "Attempting to "
            'T.:<>: 'T.Text action
            'T.:<>: 'T.Text " a field named `"
            'T.:<>: 'T.Text key
            'T.:<>: 'T.Text "' in an OpenProduct."
            'T.:$$: 'T.Text "But the OpenProduct does not have a field `"
              'T.:<>: 'T.Text key
              'T.:<>: 'T.Text "'."
        )
    )

type FindableDuring action key ts =
  ( T.KnownNat (FindElemDuring action key ts)
  , T.KnownNat (FindElem key ts)
  )

type LookupType (key :: k) (ts :: [(k, t)]) =
  F.FromMaybe F.Stuck F.=<< F.Lookup key ts

type UpdateElem (key :: T.Symbol) (t :: k) (ts :: [(T.Symbol, k)]) =
  F.SetIndex (FindElem key ts) '(key, t) ts

type DeleteElem (key :: T.Symbol) (ts :: [(T.Symbol, k)]) =
  F.Filter (F.Not F.<=< F.TyEq key F.<=< F.Fst) ts

type UpsertElem (key :: T.Symbol) (t :: k) (ts :: [(T.Symbol, k)]) =
  F.UnMaybe
    (F.Cons '(key, t) ts)
    (F.ConstFn (F.Eval (UpdateElem key t ts)))
    F.=<< FindIndex key ts

data Key (key :: T.Symbol) = Key

instance key ~ key' => IsLabel key (Key key') where
  fromLabel = Key

class MaybeIndex (m :: Maybe F.Nat) where
  maybeIndex :: Maybe Int

instance MaybeIndex 'Nothing where
  maybeIndex = Nothing

instance T.KnownNat a => MaybeIndex ('Just a) where
  maybeIndex = Just $ fromIntegral $ T.natVal $ Proxy @a

nil :: OpenProduct f '[]
nil = OpenProduct empty

insert
  :: RequireUniqueKey (F.Eval (UniqueKey key ts)) key t ts
  => Key key
  -> f t
  -> OpenProduct f ts
  -> OpenProduct f ('(key, t) ': ts)
insert _ ft (OpenProduct v) = OpenProduct $ cons (Any ft) v

findElem
  :: forall key ts
   . T.KnownNat (FindElem key ts)
  => Int
findElem = fromIntegral $ T.natVal $ Proxy @(FindElem key ts)

get
  :: forall key ts f
   . FindableDuring "get" key ts
  => Key key
  -> OpenProduct f ts
  -> f (F.Eval (LookupType key ts))
get _ (OpenProduct v) = unAny $ unsafeIndex v $ findElem @key @ts
 where
  unAny (Any a) = unsafeCoerce a

update
  :: forall key ts t f
   . FindableDuring "update" key ts
  => Key key
  -> f t
  -> OpenProduct f ts
  -> OpenProduct f (F.Eval (UpdateElem key t ts))
update _ ft (OpenProduct v) = OpenProduct $ v // [(findElem @key @ts, Any ft)]

delete
  :: forall key ts f
   . FindableDuring "delete" key ts
  => Key key
  -> OpenProduct f ts
  -> OpenProduct f (F.Eval (DeleteElem key ts))
delete _ (OpenProduct v) = OpenProduct $ flip ifilter v $ curry $ (findElem @key @ts ==) . fst

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
