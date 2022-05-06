------------------------------------------------------
--  Exercise 15.5 - Programming in Haskell - Hutton  --
------------------------------------------------------

data Tree a = Leaf | Node (Tree a) a (Tree a)
              deriving Show

treeRepeat :: a -> Tree a
treeRepeat x = Node (treeRepeat x) x (treeRepeat x)

treeTake :: Int -> Tree a -> Tree a
treeTake 0 _            = Leaf
treeTake _ Leaf         = Leaf
treeTake n (Node l x r) = Node (treeTake (n - 1) l) x (treeTake (n - 1) r)

treeReplicate :: Int -> a -> Tree a
treeReplicate 0 _ = Leaf
treeReplicate n x = Node (treeReplicate (n - 1) x) x (treeReplicate (n - 1) x)
