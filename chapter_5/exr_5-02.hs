------------------------------------------------------
--  Exercise 5.2 - Programming in Haskell - Hutton  --
------------------------------------------------------

grid :: Int -> Int -> [(Int, Int)]
grid m n    | m < 0 || n < 0      = []
            | otherwise             = [(i, j) | i <- [0..m], j <- [0..n]]
