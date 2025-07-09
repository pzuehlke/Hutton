-------------------------------------------------------
--  Exercise 7.10 - Programming in Haskell - Hutton  --
-------------------------------------------------------

toggleFunctions :: ((a -> b), (a -> b)) -> (Int, a) -> b
toggleFunctions (f, g) (n, x)   | even n    = f x
                                | otherwise = g x

altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap f g xs = map (toggleFunctions (f, g)) (zip [0..] xs)

luhnDouble :: Int -> Int
luhnDouble n | n >= 5       = 2 * n - 9
             | otherwise    = 2 * n

luhn :: [Int] -> Bool
luhn ns = ((sum (altMap id luhnDouble (reverse ns)) `mod` 10) == 0)

-- Sample tests:
-- luhn [1, 7, 8, 4] == True
-- luhn [4, 7, 8, 3] == False
