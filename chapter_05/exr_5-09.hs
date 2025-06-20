------------------------------------------------------
--  Exercise 5.9 - Programming in Haskell - Hutton  --
------------------------------------------------------

scalarProduct :: Num a => [a] -> [a] -> a
scalarProduct xs ys = sum [x * y | (x, y) <- zip xs ys]

scalarProduct' :: Num a => [a] -> [a] -> a
scalarProduct' xs ys = sum (zipWith (*) xs ys)

scalarProduct'' :: Num a => [a] -> [a] -> a
scalarProduct'' xs ys = sum (map (\(x, y) -> x * y) (zip xs ys))

