------------------------------------------------------
--  EXERCISE 4.3 - PROGRAMMING IN HASKELL - HUTTON  --
------------------------------------------------------

safeTail :: [a] -> [a]
safeTail xs = if null xs then [] else tail xs

safeTail' :: [a] -> [a]
safeTail' xs | null xs      = []
             | otherwise    = tail xs

safeTail'' :: [a] -> [a]
safeTail'' [] = []
safeTail'' (_:xs) = xs
