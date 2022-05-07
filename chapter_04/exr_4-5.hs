-------------------------------------------------------
--  EXERCISE 4.5 - PROGRAMMING IN HASKELL - HUTTON  --
-------------------------------------------------------

(&&) :: Bool -> Bool -> Bool
(&&) a b = if a == True then if b == True then True else False else False
