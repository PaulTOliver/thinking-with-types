{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

-- Exercise 10.4-i
-- Write a promoted functor instance for tuples.

import Data.Kind (Type)

type Exp a = a -> Type

type family Eval (e :: Exp a) :: a

data Map :: (a -> Exp b) -> f a -> Exp (f b)

type instance Eval (Map f '(a, b)) = '(a, Eval (f b))
