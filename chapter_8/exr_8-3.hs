------------------------------------------------------
--  Exercise 8.3 - Programming in Haskell - Hutton  --
------------------------------------------------------

data Tree a = Leaf a | Node (Tree a) (Tree a)

countLeaves :: Tree a -> Int
countLeaves (Leaf _)    = 1
countLeaves (Node l r)  = countLeaves l + countLeaves r

isBalanced :: Tree a -> Bool
isBalanced (Leaf _)     = True
isBalanced (Node l r)   = (abs (countLeaves l - countLeaves r) <= 1)
                          && isBalanced l && isBalanced r
