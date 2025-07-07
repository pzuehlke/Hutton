------------------------------------------------------
--  Exercise 7.9 - Programming in Haskell - Hutton  --
------------------------------------------------------

altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap f g xs = map (toggleFunctions (f, g)) (zip [0..] xs)

-- or, using a list comprehension:
altMap' f g xs = [toggleFunctions (f, g) (n,  x) | (n, x) <- zip [0..] xs]

toggleFunctions :: ((a -> b), (a -> b)) -> (Int, a) -> b
toggleFunctions (f, g) (n, x)   | even n    = f x
                                | otherwise = g x

-- Sample test:
-- altMap  (+10) (+100) [0, 1, 2, 3, 4] == [10, 101, 12, 103, 14]
-- altMap' (+10) (+100) [0, 1, 2, 3, 4] == [10, 101, 12, 103, 14]
