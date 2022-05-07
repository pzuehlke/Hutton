-------------------------------------------------------
--  Exercise 15.4 - Programming in Haskell - Hutton  --
-------------------------------------------------------

fibs :: [Integer]
fibs = 0 : 1 : map (uncurry (+)) (zip fibs (tail fibs))
