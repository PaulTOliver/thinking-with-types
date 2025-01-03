{-# LANGUAGE DataKinds #-}

-- Exercise 2.4-i
-- Write a closed type family to compute `Not`.

-- I use my own boolean data type and generated kinds. But it's the same as
-- just using plain `Bool`:
data MyBool
  = MyTrue
  | False

type family Not (x :: MyBool) :: MyBool where
  Not 'MyTrue = 'MyFalse
  Not 'MyFalse = 'MyTrue
