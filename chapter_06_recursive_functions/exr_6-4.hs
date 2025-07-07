------------------------------------------------------
--  EXERCISE 6.4 - PROGRAMMING IN HASKELL - HUTTON  --
------------------------------------------------------

euclid :: Int -> Int -> Int
euclid m 0      = m
euclid 0 n      = n
euclid m n      | n <= m    = euclid (m - n) n
                | otherwise = euclid m (n - m)

-- A better version:
euclid' :: Int -> Int -> Int
euclid' m 0      = m
euclid' m n      = euclid n (m `mod` n)
