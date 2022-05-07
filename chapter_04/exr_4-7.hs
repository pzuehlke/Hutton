------------------------------------------------------
--  EXERCISE 4.7 - PROGRAMMING IN HASKELL - HUTTON  --
------------------------------------------------------

{-
mult :: Int -> Int -> Int -> Int
mult x y z = x * y * z
-}

mult :: Int -> (Int -> (Int -> Int))
mult = \x -> (\y -> (\z -> x * y * z))
