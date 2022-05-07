------------------------------------------------------
--  EXERCISE 4.2 - PROGRAMMING IN HASKELL - HUTTON  --
------------------------------------------------------

third :: [a] -> a
third xs = head (tail (tail xs))

third' :: [a] -> a
third' xs = xs !! 2

third'' :: [a] -> a
third'' (_:_:x:xs)  = x
