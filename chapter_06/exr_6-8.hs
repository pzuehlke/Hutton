------------------------------------------------------
--  EXERCISE 6.8 - PROGRAMMING IN HASKELL - HUTTON  --
------------------------------------------------------

merge :: Ord a => [a] -> [a] -> [a]
merge [] ys                     = ys
merge xs []                     = xs
merge (x:xs) (y:ys) | x <= y    = x : (merge xs (y : ys))
                    | y < x     = y : (merge (x : xs) ys)

halve :: [a] -> ([a], [a])
halve xs = (take n xs, drop n xs)
    where
        n = length xs `div` 2

msort :: Ord a => [a] -> [a]
msort []    = []
msort [x]   = [x]
msort xs    = merge (msort left) (msort right)
    where
        (left, right) = halve xs
