------------------------------------------------------
--  EXERCISE 4.8 - PROGRAMMING IN HASKELL - HUTTON  --
------------------------------------------------------

luhnDouble :: Int -> Int
luhnDouble n    | m > 9     = m - 9
                | otherwise = m
    where
        m = 2 * n

-- The following function operates on lists of arbitrary size and returns the
-- checksum described in the statement. Note that the description given in the
-- statement differs from that on the Wikipedia page for the algorithm, but
-- they are essentially equivalent (in the latter description, a digit
-- coinciding with the checksum below is appended).
--
-- Note also that _every_ digit is included in the sum, but only every other is
-- doubled/slashed mod 9, starting from the second last one.
-- 
check:: [Int] -> Int
check []                             = 0
check [n]                            = n
check [m, n]                         = (luhnDouble m + n) `mod` 10
check (m:n:ns)   | even (length ns)  = (luhnDouble m + n + check ns) `mod` 10
                 | otherwise         = (m + check (n:ns)) `mod` 10

-- The following function works on lists of numbers of arbitrary size instead
-- of having four individual numbers as its parameters.
luhn :: [Int] -> Bool
luhn ns = (check ns == 0)

