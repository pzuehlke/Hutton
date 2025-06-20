------------------------------------------------------
--  EXERCISE 6.2 - PROGRAMMING IN HASKELL - HUTTON  --
------------------------------------------------------

-- Unsafe version:
sumdown :: Int -> Int
sumdown 0 = 0
sumdown n = n + sumdown (n - 1)

-- Safe version:
sumdown' :: Int -> Int
sumdown' n  | n > 0     = n + sumdown' (n - 1)
            | otherwise = 0
