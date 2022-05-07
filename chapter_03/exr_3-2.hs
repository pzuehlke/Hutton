------------------------------------------------------
--  EXERCISE 3.2 - PROGRAMMING IN HASKELL - HUTTON  --
------------------------------------------------------

bools :: [Bool]
bools = [True, False, True, False]

nums :: [[Int]]
nums = [[0], [-1, 1], [1, 2, 3]]

add :: Int -> Int -> Int -> Int
add x y z = x + y + z

copy :: a -> (a, a)
copy x = (x, x)

apply :: (a -> b) -> a -> b
apply f x = f x

