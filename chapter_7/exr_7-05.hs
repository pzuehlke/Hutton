------------------------------------------------------
--  Exercise 7.5 - Programming in Haskell - Hutton  --
------------------------------------------------------

myCurry :: ((a, b) -> c) -> (a -> b -> c)
myCurry f = \x y -> f (x, y)

myUncurry :: (a -> b -> c) -> ((a, b) -> c)
myUncurry f = \(x, y) -> f x y

-- Functions to test the two preceding ones on:
-- myCurry add = plus and myUncrury plus = add
add :: (Int, Int) -> Int
add (x, y) = x + y

plus :: Int -> Int -> Int
plus x y = x + y
