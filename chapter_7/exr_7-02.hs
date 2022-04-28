------------------------------------------------------
--  Exercise 7.2 - Programming in Haskell - Hutton  --
------------------------------------------------------

-- (a)
myAll :: (a -> Bool) -> [a] -> Bool
myAll p = and . (map p)

-- (b)
myAny :: (a -> Bool) -> [a] -> Bool 
myAny p = or . (map p)

-- (c)
myTakeWhile :: (a -> Bool) -> [a] -> [a]
myTakeWhile _ []                    = []
myTakeWhile p (x:xs)    | p x       = x : myTakeWhile p xs
                        | otherwise = []

-- (d)
myDropWhile :: (a -> Bool) -> [a] -> [a]
myDropWhile _ []                    = []
myDropWhile p (x:xs)    | p x       = myDropWhile p xs
                        | otherwise = x : xs

