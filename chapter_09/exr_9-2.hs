------------------------------------------------------
--  Exercise 9.2 - Programming in Haskell - Hutton  --
------------------------------------------------------

-- | Given two lists, decides if the first can be obtained from the second by
-- omitting some of its elements, in a possibly different order.
isChoice :: Eq a => [a] -> [a] -> Bool
isChoice [] _                       = True
isChoice (x:xs) ys  | x `elem` ys   = isChoice xs (remove x ys)
                    | otherwise     = False

-- | Removes the first instance of a given object from a given list in case
-- this object is in fact an element of the list; if it is not, returns the
-- original list.
remove :: Eq a => a -> [a] -> [a]
remove x []     = []
remove x (y:ys) | x == y    = ys
                | otherwise = y : (remove x ys)
