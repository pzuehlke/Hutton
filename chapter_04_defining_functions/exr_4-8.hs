------------------------------------------------------
--  EXERCISE 4.8 - PROGRAMMING IN HASKELL - HUTTON  --
------------------------------------------------------

-- The following function operates on lists of arbitrary size.
-- Note that the description given in the statement differs from that on the
-- Wikipedia page for the algorithm, but they are essentially equivalent (in
-- the latter description, a digit coinciding with the checksum below is
-- appended).

-- Note also that _every_ digit is included in the sum, but only every other is
-- doubled/slashed mod 9, starting from the penultimate one.

luhnDouble :: Int -> Int
luhnDouble n    | 2 * n > 9 = 2 * n - 9
                | otherwise = 2 * n

luhnSum :: [Int] -> Int
luhnSum []                              = 0
luhnSum [n]                             = n
luhnSum (m:n:ns)    | even (length ns)  = luhnDouble m + n + luhnSum ns
                    | otherwise         = m + luhnSum (n:ns)


luhn :: [Int] -> Bool
luhn ns = luhnSum ns `mod` 10 == 0
