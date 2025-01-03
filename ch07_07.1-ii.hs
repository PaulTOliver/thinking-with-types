-- Exercise 7.1-ii
-- What happens to this instance if you remove the `Show t =>` constraint from
-- `HasShow`.

-- Removing `Show t =>` below makes GHC angry:
--
-- • No instance for ‘Show t’ arising from a use of ‘show’
--   Possible fix:
--     add (Show t) to the context of the data constructor ‘HasShow’
data HasShow where
  HasShow :: Show t => t -> HasShow

instance Show HasShow where
  show (HasShow t) = "HasShow " <> show t
