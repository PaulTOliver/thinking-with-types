{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

-- Exercise 10.2-i
-- Defunctionalize `listToMaybe` at the type-level.

import Data.Kind (Type)

type Exp a = a -> Type

type family Eval (e :: Exp a) :: a

data ListToMaybe :: [a] -> Exp (Maybe a)

type instance Eval (ListToMaybe '[]) = 'Nothing

type instance Eval (ListToMaybe (x ': _)) = 'Just x
