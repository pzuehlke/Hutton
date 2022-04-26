------------------------------------------------------
--  EXERCISE 2.4 - PROGRAMMING IN HASKELL - HUTTON  --
------------------------------------------------------

myLast :: [a] -> a
myLast xs = xs !! (length xs - 1)

myLast' :: [a] -> a
myLast' = head . reverse

myLast'' :: [a] -> Maybe a
myLast'' []     = Nothing
myLast'' [x]    = Just x
myLast'' (x:xs) = myLast'' xs
