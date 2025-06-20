------------------------------------------------------
--  EXERCISE 4.2 - PROGRAMMING IN HASKELL - HUTTON  --
------------------------------------------------------

-- (a)
third :: [a] -> a
third xs = head (tail (tail xs))

-- (b)
third' :: [a] -> a
third' xs = xs !! 2

-- (c)
third'' :: [a] -> a
third'' (_:_:x:xs)  = x
