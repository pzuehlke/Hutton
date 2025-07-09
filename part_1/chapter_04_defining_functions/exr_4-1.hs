------------------------------------------------------
--  EXERCISE 4.1 - PROGRAMMING IN HASKELL - HUTTON  --
------------------------------------------------------

halve :: [a] -> ([a], [a])
halve xs = (take half xs, drop half xs)
    where half = length xs `div` 2 

halve' :: [a] -> ([a], [a])
halve' xs = splitAt (length xs `div` 2) xs
