{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

import Data.Constraint (Dict (Dict))
import Data.Kind (Type)
import Data.Singletons (Demote, Sing, SingI, SingKind, fromSing, sing)
import Data.Singletons.Decide qualified as D

data Sigma (f :: k -> Type) where
  Sigma :: Sing a -> f a -> Sigma f

withSigma :: (forall (a :: k). Sing a -> f a -> r) -> Sigma f -> r
withSigma c (Sigma s f) = c s f

toSigma :: SingI a => f a -> Sigma f
toSigma = Sigma sing

fromSigma
  :: forall k (a :: k) (f :: k -> Type)
   . (SingI a, D.SDecide k)
  => Sigma f
  -> Maybe (f a)
fromSigma (Sigma s f) =
  case s D.%~ sing @a of
    D.Proved D.Refl -> Just f
    D.Disproved _ -> Nothing

class Dict1 c (f :: k -> Type) where
  dict1 :: Sing (a :: k) -> Dict (c (f a))

instance
  ( Dict1 Eq (f :: k -> Type)
  , D.SDecide k
  )
  => Eq (Sigma f)
  where
  Sigma sa fa == Sigma sb fb =
    case sa D.%~ sb of
      D.Proved D.Refl ->
        case dict1 @_ @Eq @f sa of
          Dict -> fa == fb
      D.Disproved _ -> False

-- Exercise 15.5-i
-- Provide an instance of `Ord` for `Sigma` by comparing the `fs` if the
-- singletons are equal, comparing the singletons at the term-level otherwise.
instance
  ( Dict1 Eq (f :: k -> Type)
  , Dict1 Ord (f :: k -> Type)
  , D.SDecide k
  , Ord (Demote k)
  , SingKind k
  )
  => Ord (Sigma f)
  where
  compare (Sigma sa fa) (Sigma sb fb) =
    case sa D.%~ sb of
      D.Proved D.Refl ->
        -- Sandy passes `@Ord` as the first type application, however that
        -- failed to compile on my PC. I had to do `@_ @Ord` instead (same
        -- with `@Eq` above and `@Show` below). I wonder if newer versions of
        -- GHC treat the order of type variables differently when used against
        -- `class` functions, like `dict1` above.
        case dict1 @_ @Ord @f sa of
          Dict -> compare fa fb
      D.Disproved _ -> compare (fromSing sa) (fromSing sb)

instance
  ( Dict1 Show (f :: k -> Type)
  , Show (Demote k)
  , SingKind k
  )
  => Show (Sigma f)
  where
  show (Sigma sa fa) =
    case dict1 @_ @Show @f sa of
      Dict ->
        mconcat
          [ "Sigma "
          , show $ fromSing sa
          , " ("
          , show fa
          , ")"
          ]
