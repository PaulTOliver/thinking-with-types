-- Exercise 7.1-iii
-- Write the `Show` instance for `HasShow` in terms of `elimHasShow`.
data HasShow where
  HasShow :: Show t => t -> HasShow

elimHasShow :: (forall a. Show a => a -> r) -> HasShow -> r
elimHasShow f (HasShow a) = f a

-- This version also prepends the `HasShow` name to the result of `show`:
instance Show HasShow where
  show = mappend "HasShow " . elimHasShow show
