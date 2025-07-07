-------------------------------------------------------
--  Exercise 15.4 - Programming in Haskell - Hutton  --
-------------------------------------------------------

-- The idea is that, by the formula defining the Fibonacci sequence:
--   [0, 1,     1, 2, 3, 5, 8, ...]
-- = [0, 1] ++ [0, 1, 1, 2, 3, ...] (this is 0 : 1 : fibs)
--           + [1, 1, 2, 3, 5, ...] (this is tail fibs)

-- Using a list comprehension:
fibs :: [Integer]
fibs = 0 : 1 : [m + n | (m, n) <- zip fibs (tail fibs)]

-- Alternatively, using zipWith:
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
