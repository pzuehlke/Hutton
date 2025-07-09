-------------------------------------------------------
--  Luhn algorithm for verification of card numbers  --
-------------------------------------------------------

-- | Apply one of two functions f or g according to whether n is even or not.
toggleFunctions :: ((a -> b), (a -> b)) -> Int -> a -> b
toggleFunctions (f, g) n x  | even n        = f x
                            | otherwise     = g x

-- | Alternate between applications of f and g on the elements of a list.
altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap f g xs = [toggleFunctions (f, g) n x | (x, n) <- zip xs [0..]]

-- | If a digit d is < 5, return 2 * d; otherwise, return 2 * d - 9.
luhnDouble :: Int -> Int
luhdDouble d    | d >= 5        = 2 * d - 9
                | otherwise     = 2 * d

-- | Verifies whether a card number, given as a list of digits, is valid or not
-- according to the description of the Luhn algorithm in the book.
luhn :: [Int] -> Bool
luhn ns = ((sum (altMap id luhnDouble (reverse ns)) `mod` 10) == 0)
