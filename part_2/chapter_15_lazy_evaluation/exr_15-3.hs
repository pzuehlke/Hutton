-------------------------------------------------------
--  Exercise 15.3 - Programming in Haskell - Hutton  --
-------------------------------------------------------

-- Assuming that `mult` is defined by:
mult :: Num a => a -> (a -> a)
mult = \x -> (\y -> x * y)

--      mult 3 4            {application of mult to 3}
-- =    (\y -> 3 * y) 4     {application of the lambda to 4} 
-- =    3 * 4               {application of (*)}
-- =    12
