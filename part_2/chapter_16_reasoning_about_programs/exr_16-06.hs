-------------------------------------------------------
--  Exercise 16.6 - Programming in Haskell - Hutton  --
-------------------------------------------------------

data Tree = Leaf Int | Node Tree Tree

data Tree = Leaf Int | Node Tree Tree

countLeaves :: Tree -> Int
countLeaves (Leaf _)    = 1
countLeaves (Node l r)  = countLeaves l + countLeaves r

countNodes :: Tree -> Int
countNodes (Leaf _)     = 0
countNodes (Node l r)   = 1 + countNodes l + countNodes r

-- Theorem: For any tree t, countLeaves t = 1 + countNodes t.
--
-- Proof: By induction on the depth of the tree.
--
-- Base case (t = Leaf_):
--      countLeaves (Leaf _)    {applying countLeaves}
--    = 1                       {0 is a neutral element for +}
--    = 1 + 0                   {unapplying countNodes}
--    = 1 + countNodes (Leaf _)
--
-- Inductive case (t = Node l r):
--      countLeaves (Node l r)                      {applying countLeaves}
--    = countLeaves l + countLeaves r               {induction hypothesis}
--    = (1 + countNodes l) + (1 + countNodes r)     {arithmetic}
--    = 1 + (1 + countNodes l + countNodes r)       {unapplying countNodes}
--    = 1 + countNodes (Node l r)         []
