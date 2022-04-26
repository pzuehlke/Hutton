------------------------------------------------------
--  EXERCISE 4.3 - PROGRAMMING IN HASKELL - HUTTON  --
------------------------------------------------------

myProduct :: Num a => [a] -> a
myProduct []        = 1
myProduct (x:xs)    = x * myProduct xs

{--
myProduct [2, 3, 4] -> 2 * (myProduct [3, 4]) -> (2 * (3 * myProduct [4]))
-> (2 * (3 * (4 * myProduct []))) -> (2 * (3 * (4 * 1))) -> (2 * (3 * 4))
-> (2 * (12)) -> 24
--}
