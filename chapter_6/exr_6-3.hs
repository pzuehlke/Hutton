------------------------------------------------------
--  EXERCISE 6.3 - PROGRAMMING IN HASKELL - HUTTON  --
------------------------------------------------------

myExp :: Int -> Int -> Int
myExp a 0     = 1
myExp a n     = a * (myExp a (n - 1))
