------------------------------------------------------
--  Exercise 8.4 - Programming in Haskell - Hutton  --
------------------------------------------------------

data Tree a = Leaf a | Node (Tree a) (Tree a) deriving Show

halve :: [a] -> ([a], [a])
halve [] = ([], [])
halve xs = (ls, rs) where
    ls = take h xs
    rs = drop h xs
    h  = length xs `div` 2

balance :: [a] -> Tree a
balance [x] = Leaf x
balance xs  = Node (balance ls) (balance rs)
    where (ls, rs) = halve xs

-- Sample tests:
-- isBalanced (balance [1, 2, 3, 4])
-- isBalanced (balance [1])
-- isBalanced (balance [1, 2, 3])
-- isBalanced (balance [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11])
-- where isBalanced was defined in exr 8.3.

countLeaves :: Tree a -> Int
countLeaves (Leaf _)    = 1
countLeaves (Node l r)  = countLeaves l + countLeaves r

isBalanced :: Tree a -> Bool
isBalanced (Leaf _)     = True
isBalanced (Node l r)   = (abs (countLeaves l - countLeaves r) <= 1)
                          && isBalanced l && isBalanced r
