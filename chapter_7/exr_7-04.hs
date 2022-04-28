------------------------------------------------------
--  Exercise 7.4 - Programming in Haskell - Hutton  --
------------------------------------------------------

decToInt :: [Int] -> Int
decToInt = foldl (\a b -> 10 * a + b) 0
