------------------------------------------------------
--  Exercise 7.1 - Programming in Haskell - Hutton  --
------------------------------------------------------

-- Answer: [f x | x <- xs, p x] = comprehension xs, where:
comprehension :: (a -> b) -> (a -> Bool) -> [a] -> [b]
comprehension f p = map f . filter p
