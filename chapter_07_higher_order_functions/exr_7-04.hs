------------------------------------------------------
--  Exercise 7.4 - Programming in Haskell - Hutton  --
------------------------------------------------------

decToInt :: [Int] -> Int
decToInt = foldl (\n digit -> 10 * n + digit) 0
