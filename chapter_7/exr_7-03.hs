------------------------------------------------------
--  Exercise 7.3 - Programming in Haskell - Hutton  --
------------------------------------------------------

myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr (\x xs -> (f x : xs)) []

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter p = foldr f []
             where
               f x xs    | p x       = x : xs
                            | otherwise = xs
    
