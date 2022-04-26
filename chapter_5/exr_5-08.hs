------------------------------------------------------
--  Exercise 5.8 - Programming in Haskell - Hutton  --
------------------------------------------------------

find :: Eq a => a -> [(a, b)] -> [b]
find k tuples = [value | (key, value) <- tuples, key == k]

positions :: Eq a => a -> [a] -> [Int]
positions x xs = find x (zip xs [0..m])
                    where m = length xs - 1

