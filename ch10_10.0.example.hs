{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

import Data.Kind (Type)

type Exp a = a -> Type

type family Eval (e :: Exp a) :: a

data Fst :: (a, b) -> Exp a

data Snd :: (a, b) -> Exp b

type instance Eval (Fst '(a, b)) = a

type instance Eval (Snd '(a, b)) = b
