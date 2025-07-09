-------------------------------------------------------
--  Exercise 15.6 - Programming in Haskell - Hutton  --
-------------------------------------------------------

eps :: Double
eps = 0.00001
initial :: Double
initial = 1.0

-- | Custom version of iterate, for practice:
-- iterate f x = [x, f x, f (f x), f (f (f x)), ...]
iterate' :: (a -> a) -> a -> [a]
iterate' f x = x : map f (iterate' f x)
-- or:
-- iterate' f x = x : iterate' f (f x)

-- | Calculates an approximation to the square root of a floating-point number
-- using Heron's (a special case of Newton's) method. Begins with `initial` as
-- the first approximation and stops when the two consecutive approximations
-- differ by less than `eps`. 
sqroot :: Double -> Double
sqroot x = (fst . head) [(a, e) | (a, e) <- zip approx errors, e < eps] where
-- or:   = (fst . head) (filter (\(_, e) -> e < eps) (zip approx errors)) where
    approx  = iterate next initial
    next    = \a -> (a + x / a) / 2
    errors  = zipWith (\a b -> abs(a - b)) approx (tail approx)

-- Sample tests:
-- abs (sqroot 3 - 1.73205080757) < 0.00001
-- abs (sqroot 2 - 1.41421356237) < 0.00001
