------------------------------------------------------
--  Exercise 7.2 - Programming in Haskell - Hutton  --
------------------------------------------------------

-- Note the typos in the statements of (a) and (b): the second parameter type
-- should be [a], not [Bool].
-- (a)
myAll :: (a -> Bool) -> [a] -> Bool
myAll p = foldr (&&) True . map p
-- Alternatively:
myAll' p = and . map p
-- Or, without using higher-order functions:
myAll'' _ []                    = True
myAll'' p (x:xs) | p x          = myAll' p xs
                 | otherwise    = False

-- (b)
myAny :: (a -> Bool) -> [a] -> Bool 
myAny p = foldr (||) False . map p
-- Alternatively:
myAny' p = or . (map p)
-- Or, without using higher-order functions:
myAny'' _ []                 = False
myAny'' p (x:xs) | p x       = True
                | otherwise = myAny'' p xs

-- (c)
myTakeWhile :: (a -> Bool) -> [a] -> [a]
myTakeWhile p = foldr (\x xs -> if p x then x : xs else []) []
-- Or, without using higher-order functions:
myTakeWhile' _ []                   = []
myTakeWhile' p (x:xs)   | p x       = x : myTakeWhile' p xs
                        | otherwise = []

-- (d)
myDropWhile :: (a -> Bool) -> [a] -> [a]
myDropWhile p = foldl (\xs x -> if p x && null xs then [] else xs ++ [x]) []
-- Or, without using higher-order functions:
myDropWhile' _ []                   = []
myDropWhile' p (x:xs)   | p x       = myDropWhile' p xs
                        | otherwise = x : xs
