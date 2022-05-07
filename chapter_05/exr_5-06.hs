------------------------------------------------------
--  Exercise 5.6 - Programming in Haskell - Hutton  --
------------------------------------------------------

factors :: Int -> [Int]
factors n = [d | d <- [1..n], n `mod` d == 0]

perfects :: Int -> [Int]
perfects n = [m | m <- [1..n], sum (factors m) == 2 * m]
