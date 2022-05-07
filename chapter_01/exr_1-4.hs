------------------------------------------------------
--  EXERCISE 1.4 - PROGRAMMING IN HASKELL - HUTTON  --
------------------------------------------------------

reverseQSort :: Ord a => [a] -> [a]
reverseQSort []     = [] 
reverseQSort (x:xs) = reverseQSort larger ++ [x] ++ reverseQSort smaller
                      where
                        larger = [y | y <- xs, y >= x]
                        smaller = [y | y <- xs, y < x]
