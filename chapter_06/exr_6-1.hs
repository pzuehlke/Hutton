------------------------------------------------------
--  EXERCISE 6.1 - PROGRAMMING IN HASKELL - HUTTON  --
------------------------------------------------------

-- If factorial is called on a negative argument such as -1, then it will cause
-- a stack overflow error (an infinite sequence of function calls).
fac :: Int -> Int
fac n   | n > 0        = n * fac (n - 1)
        | otherwise    = 1
