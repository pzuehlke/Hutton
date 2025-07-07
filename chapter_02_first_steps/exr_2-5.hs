------------------------------------------------------
--  EXERCISE 2.5 - PROGRAMMING IN HASKELL - HUTTON  --
------------------------------------------------------

myInit :: [a] -> [a]
myInit []   = []
myInit xs   = take (length xs - 1) xs 

myInit' :: [a] -> [a]
myInit' = reverse . tail . reverse

myInit'' :: [a] -> [a]
myInit'' []     = []
myInit'' [x]    = []
myInit'' (x:xs) = x : (myInit'' xs)
