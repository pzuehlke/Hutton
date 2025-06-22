------------------------------------------------------
--  Exercise 7.1 - Programming in Haskell - Hutton  --
------------------------------------------------------

-- Answer: [f x | x <- xs, p x] = comprehension f p xs, where:
comprehension :: (a -> b) -> (a -> Bool) -> [a] -> [b]
comprehension f p = map f . filter p
