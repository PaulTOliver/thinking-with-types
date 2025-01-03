-- Exercise 15.4-i
-- Give instances of `SDecide` for `Maybe`.

-- This is slightly different to Sandy's version but I feel it should work the
-- same. I can't easily test if it compiles though. Importing
-- `Data.Singletons.Decide` pulls in the already existing instances!
instance SDecide a => SDecide (Maybe a) where
  SJust a %~ SJust b = a %~ b
  SNothing %~ SNothing = Proved Refl
  _ %~ _ = Disproved $ const undefined
