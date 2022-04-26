------------------------------------------------------
--  EXERCISE 6.9 - PROGRAMMING IN HASKELL - HUTTON  --
------------------------------------------------------

-- (a)
mySum :: Num a => [a] -> a
mySum []        = 0
mySum (x:xs)    = x + mySum xs

-- (b)
myTake :: Int -> [a] -> [a]
myTake n []                 = []
myTake n (x:xs) | n <= 0    = []
                | otherwise = x : myTake (n - 1) xs
-- (c) (this definition does not cover the case where the list is empty) myLast :: [a] -> a
myLast [x]      = x
myLast (x:xs)   = myLast xs
