------------------------------------------------------
--  Exercise 8.4 - Programming in Haskell - Hutton  --
------------------------------------------------------

data Tree a = Leaf a | Node (Tree a) (Tree a) deriving Show

halve :: [a] -> ([a], [a])
halve []    = ([], [])
halve xs    = (ls, rs)
              where
                ls = take m xs
                rs = drop m xs
                m  = length xs `div` 2

balance :: [a] -> Tree a
balance [x]     = Leaf x
balance xs      = Node (balance ls) (balance rs)
                  where
                    (ls, rs) = halve xs

-- Sample test: isBalanced (balance [1, 2, 3, 4])
-- where isBalanced was defined in exr 8.3.
