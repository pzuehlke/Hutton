------------------------------------------------------
--  Exercise 5.3 - Programming in Haskell - Hutton  --
------------------------------------------------------

grid :: Int -> Int -> [(Int, Int)]
grid m n = [(i, j) | i <- [0..m], j <- [0..n]]

square :: Int -> [(Int, Int)]
square n = [(i, j) | (i, j) <- grid n n, i /= j]
