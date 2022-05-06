------------------------------------------------------
--  Exercise 15.3 - Programming in Haskell - Hutton  --
------------------------------------------------------

-- mult = \x -> (\y -> x * y)
-- mult 3 4 -> (\x -> (\y -> x * y)) 3 4 {application of mult}
--          -> (\y -> 3 * y) 4 {application of the x-lambda to 3}
--          -> 3 * 4 {application of the y-lambda to 4} 
--          -> 12 {application of (*)}
