------------------------------------------------------
--  EXERCISE 1.5 - PROGRAMMING IN HASKELL - HUTTON  --
------------------------------------------------------

-- Answer: The result would be that the function will return a sorted list with
-- all duplicates removed.

quickSort :: Ord a => [a] -> [a]
quickSort []        = []
quickSort (x:xs)    = quickSort smaller ++ [x] ++ quickSort larger
    where
        smaller = [y | y <- xs, y < x]
        larger  = [y | y <- xs, y > x]
