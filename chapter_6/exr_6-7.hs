------------------------------------------------------
--  EXERCISE 6.7 - PROGRAMMING IN HASKELL - HUTTON  --
------------------------------------------------------

merge :: Ord a => [a] -> [a] -> [a]
merge [] ys                     = ys
merge xs []                     = xs
merge (x:xs) (y:ys) | x <= y    = x : (merge xs (y : ys))
                    | y < x     = y : (merge (x : xs) ys)
