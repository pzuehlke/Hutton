-------------------------------------------------------
--  EXERCISE 14.4 - PROGRAMMING IN HASKELL - HUTTON  --
-------------------------------------------------------

data Tree a = Leaf | Node (Tree a) a (Tree a) deriving Show

instance Foldable Tree where
    -- fold :: Monoid a => Tree a -> a
    fold Leaf       = mempty
    fold (Node l x r) = (fold l) `mappend` x `mappend` fold r

    -- foldMap :: Monoid b => (a -> b) -> Tree a -> b
    foldMap _ Leaf          = mempty
    foldMap f (Node l x r)  =
        (foldMap f l) `mappend` (f x) `mappend` (foldMap f r)

    -- foldr :: (a -> b -> b) -> b -> Tree a -> b
    foldr _ v Leaf          = v
    foldr f v (Node l x r)  = foldr f (f x (foldr f v r)) l

    -- foldr :: (a -> b -> a) -> a -> Tree b -> a
    foldl _ v Leaf          = v
    foldl f v (Node l x r)  = foldl f (f (foldl f v l) x) r

instance Traversable Tree where
    -- traverse :: Applicative F => (a -> F b) -> Tree a -> F (Tree b)
    traverse _ Leaf         = pure Leaf
    traverse f (Node l x r) =
        pure Node <*> traverse f l <*> f x <*> traverse f r
