-- Exercise 1.4-iii
-- Prove `(a * b)^c == a^c * b*c`.
func2Tup :: (c -> (a, b)) -> (c -> a, c -> b)
func2Tup f = (fst . f, snd . f)

-- This version takes in a tuple of functions, which is equivalent to them
-- being separate arguments:
tup2Func :: (c -> a, c -> b) -> c -> (a, b)
tup2Func (f, g) v = (f v, g v)

-- This version has the same signature as Sandy's version, and calls `curry`
-- against my tupled rendition above:
tup2Func' :: (c -> a) -> (c -> b) -> c -> (a, b)
tup2Func' = curry tup2Func
