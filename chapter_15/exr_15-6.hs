-------------------------------------------------------
--  Exercise 15.6 - Programming in Haskell - Hutton  --
-------------------------------------------------------

-- Given x, returns a function which computes the next approximation given the
-- current approximation to the square root of x.
next :: Double -> Double -> Double
next x a = (a + x/a) / 2

-- Calculates an approximation to the square root of a floating-point number x
-- using Newton's approximation within an error of 0.00001.
sqroot :: Double -> Double
sqroot x = (snd . head . (filter p)) (zip as (tail as))
           where
             p (x, y) = (abs (x - y) <= 0.00001)
             as = iterate (next x) 1.0

-- Sample tests:
-- abs (sqroot 3 - 1.73205080757) < 0.00001
-- abs (sqroot 2 - 1.41421356237) < 0.00001
