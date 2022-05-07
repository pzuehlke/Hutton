------------------------------------------------------
--  EXERCISE 6.2 - PROGRAMMING IN HASKELL - HUTTON  --
------------------------------------------------------

sumdown :: Int -> Int
sumdown 0               = 0
sumdown n   | n > 0     = n + sumdown (n - 1)
            | otherwise = n + sumdown (n + 1)
