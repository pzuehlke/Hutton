------------------------------------------------------
--  Exercise 7.2 - Programming in Haskell - Hutton  --
------------------------------------------------------

-- (a)
myAll :: (a -> Bool) -> [a] -> Bool
myAll p = and . (map p)
-- Alternatively:
myAll' _ []     = True
myAll' p (x:xs) | p x       = myAll' p xs
                | otherwise = False

-- (b)
myAny :: (a -> Bool) -> [a] -> Bool 
myAny p = or . (map p)
-- Alternatively:
myAny' _ []     = False
myAny' p (x:xs) | p x       = True
                | otherwise = myAny' p xs

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
