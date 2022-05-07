------------------------------------------------------
--  EXERCISE 6.1 - PROGRAMMING IN HASKELL - HUTTON  --
------------------------------------------------------

fac :: Int -> Int
fac 0 = 1
fac n   | n > 0         = n * fac (n - 1)
        | otherwise     = n
