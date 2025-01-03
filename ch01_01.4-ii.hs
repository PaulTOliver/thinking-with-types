-- Exercise 1.4-ii
-- Give a prove of the exponent law that `a^b * a^c == a^(b+c)`.
tup2Either :: (b -> a, c -> a) -> Either b c -> a
tup2Either (f, _) (Left b) = f b
tup2Either (_, g) (Right c) = g c

either2Tup :: (Either b c -> a) -> (b -> a, c -> a)
either2Tup f = (f . Left, f . Right)
