------------------------------------------------------
--  Exercise 5.9 - Programming in Haskell - Hutton  --
------------------------------------------------------

scalarProduct :: [Int] -> [Int] -> Int
scalarProduct xs ys = sum [x * y | (x, y) <- zip xs ys]
