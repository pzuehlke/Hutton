------------------------------------------------------
--  Exercise 5.4 - Programming in Haskell - Hutton  --
------------------------------------------------------

myReplicate :: Int -> a -> [a]
myReplicate n x = [x | _ <- [1..n]]


