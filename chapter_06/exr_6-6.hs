------------------------------------------------------
--  EXERCISE 6.6 - PROGRAMMING IN HASKELL - HUTTON  --
------------------------------------------------------

-- (a)
myAnd :: [Bool] -> Bool
myAnd [] = True
myAnd (x:xs) = x && myAnd xs

-- (b)
myConcat :: [[a]] -> [a]
myConcat []         = []
myConcat [xs]       = xs
myConcat (xs : ys)  = xs ++ myConcat ys

-- (c)
myReplicate :: Int -> a -> [a]
myReplicate n x | n <= 0    = []
                | otherwise = x : myReplicate (n - 1) x

-- (d) (this definition does not cover all cases (namely, n < 0 or xs = []))
myGet :: [a] -> Int -> a
myGet (x:xs) 0  = x
myGet (x:xs) n  = myGet xs (n - 1)

-- (e)
myElem :: Eq a => a -> [a] -> Bool
myElem _ []     = False
myElem y (x:xs) = (y == x || myElem y xs)
