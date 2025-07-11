------------------------------------------------------
--  Exercise 12.1 - Programming in Haskell - Hutton  --
------------------------------------------------------

data Tree a = Leaf | Node (Tree a) a (Tree a) deriving Show

instance Functor Tree where
    -- fmap :: (a -> b) -> Tree a -> Tree b
    fmap _ Leaf         = Leaf
    fmap g Node (l x r) = Node (fmap g l) (g x) (fmap g r)
