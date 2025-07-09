------------------------------------------------------
--  Exercise 5.7 - Programming in Haskell - Hutton  --
------------------------------------------------------

comb :: [a] -> [b] -> [(a, b)]
comb xs ys = concat [[(x, y) | y <- ys] | x <- xs]

-- The list in the statement would then be given by:
-- comb [1, 2] [3, 4] = [(1, 3), (1, 4), (2, 3), (2, 4)]
