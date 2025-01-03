{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- Exercise 10.2-ii
-- Defunctionalize `foldr :: (a -> b -> b) -> b -> [a] -> b`.

import Data.Kind (Type)

type Exp a = a -> Type

type family Eval (e :: Exp a) :: a

data Foldr :: (a -> b -> Exp b) -> b -> [a] -> Exp b

type instance Eval (Foldr f z '[]) = z

type instance Eval (Foldr f z (x ': xs)) = Eval (f x (Eval (Foldr f z xs)))
