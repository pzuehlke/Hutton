------------------------------------------------------
--  Exercise 8.2 - Programming in Haskell - Hutton  --
------------------------------------------------------

-- data Ordering = LT | EQ | GT

-- compare :: Ord a => a -> a -> Ordering

data Tree a = Leaf a | Node (Tree a) a (Tree a)

-- Assuming that all trees in which the search will take place are search
-- trees, we can use the following function to search for a value:
occurs :: Ord a => a -> Tree a -> Bool
occurs x (Leaf y)       = x == y
occurs x (Node l y r)   = case compare x y of
                            LT -> occurs x l
                            EQ -> True
                            GT -> occurs x r

-- To answer the question posed at the end of the statement: It is hard to know
-- which function is more efficient without knowing more about how GHC works.
-- However, the preceding function only compares x and y once, and then returns
-- the answer depending on which of only three possibilities takes place. In
-- the original definition, x and y is compared to y _twice_ (unless x == y). I
-- suppose that if, e.g., the two integers are large, then the preceding
-- function would be more efficient for this reason.
