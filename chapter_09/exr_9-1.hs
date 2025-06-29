------------------------------------------------------
--  Exercise 9.1 - Programming in Haskell - Hutton  --
------------------------------------------------------

-- | Given a list, returns all possible sublists which can be formed by
-- selecting some (or none) of its elements, possibly in a different order.
-- Example: choices [1, 2] = [[], [1], [2], [1, 2], [2, 1]]
choices :: [a] -> [[a]]
choices xs = [perm | sublist <- sublists xs, perm <- permutations sublist]

-- | Given a list, returns all possible sublists which can be formed from it by
-- deleting some of the elements while preserving the original order.
-- Example: sublists [1, 2] = [[], [1], [2]]
sublists :: [a] -> [[a]]
sublists []         = [[]]
sublists (x:xs)     = (sublists xs) ++ (map (x:) (sublists xs))

-- | Given x and and a list xs, returns a list of lists comprising all possible
-- ways to obtain a new list by inserting x into xs.
interleave :: a -> [a] -> [[a]]
interleave x []     = [[x]]
interleave x (y:ys) = (x:y:ys) : (map (y:) (interleave x ys))

-- | Given a list, returns a list of lists comprising all permutations of its
-- elements.
permutations :: [a] -> [[a]]
permutations []        = [[]]
permutations (x:xs)    = concat (map (interleave x) (permutations xs))
