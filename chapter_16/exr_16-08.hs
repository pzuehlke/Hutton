-------------------------------------------------------
--  Exercise 16.8 - Programming in Haskell - Hutton  --
-------------------------------------------------------

data Tree a = Leaf a | Node (Tree a) (Tree a)

instance Functor Tree where
  -- fmap :: (a -> b) -> Tree a -> Tree b
  fmap g (Leaf x)    = Leaf (g x)
  fmap g (Node l r)  = Node (fmap g l) (fmap g r)

-- Theorem: (i)     fmap id = id;
--          (ii)    fmap (g . f) = (fmap g) . (fmap f).
--
-- Proof:
-- (i) By induction on the tree t:
--      Base case (t = Leaf x):
--      fmap id (Leaf x)    = {applying fmap} Leaf (id x)
--                          = {applying id} Leaf x
--
--      Inductive case (t = Node l r):
--      fmap id (Node l r)  = {applying fmap} Node (fmap id l) (fmap id r)
--                          = {induction hypothesis} Node l r               []
--
-- (ii) Also by induction on the tree t:
--      Base case (t = Leaf x):
--      fmap (g . f) (Leaf x)   = {applying fmap} Leaf ((g . f) x)
--                              = {applying .} Leaf (g (f x))
--                              = {unapplying fmap} fmap g (Leaf (f x))
--                              = {unapplying fmap} fmap g (fmap f (Leaf x))
--                              = {unapplying .} (fmap g . fmap f) (Leaf x)
--
--      Inductive case (t = Node l r):
--      fmap (g . f) (Node l r)
--      = {applying fmap} Node (fmap (g . f) l) (fmap (g . f) r)
--      = {ind. hyp.} Node (((fmap g) . (fmap f)) l) (((fmap g) . (fmap f)) r)
--      = {unapplying fmap} fmap g (Node (fmap f l) (fmap f r))
--      = {unapplying fmap} ((fmap g) . (fmap f)) (Node l r)                []
