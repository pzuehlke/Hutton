------------------------------------------------------
--  EXERCISE 6.4 - PROGRAMMING IN HASKELL - HUTTON  --
------------------------------------------------------

euclid :: Int -> Int -> Int
euclid m 0      = m
euclid 0 n      = n  
euclid m n  | m >= n    = euclid n (m - n)
            | otherwise = euclid m (n - m)
