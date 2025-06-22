------------------------------------------------------
--  Exercise 7.3 - Programming in Haskell - Hutton  --
------------------------------------------------------

myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr (\x xs -> f x : xs) []

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter p = foldr (\x xs -> if p x then x : xs else xs) []
