------------------------------------------------------
--  Exercise 5.4 - Programming in Haskell - Hutton  --
------------------------------------------------------

replicate' :: Int -> a -> [a]
replicate' n x = [x | _ <- [1..n]]
