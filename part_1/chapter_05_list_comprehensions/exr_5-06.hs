------------------------------------------------------
--  Exercise 5.6 - Programming in Haskell - Hutton  --
------------------------------------------------------

-- | Generate the list of all the factors of n, excluding n itself.
factors :: Int -> [Int]
factors n = [d | d <- [1..n `div` 2], n `mod` d == 0]

-- | Generate the list of all perfect numbers from 1 to n.
perfects :: Int -> [Int]
perfects n = [m | m <- [1..n], sum (factors m) == m]
