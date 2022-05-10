----------------------------------------------------
--  Chapter 15 - Programming in Haskell - Hutton  --
----------------------------------------------------

-- Section 4.1
{--

class Monoid a where
  mempty :: a
  mappend :: a -> a -> a

  mconcat :: [a] -> a
  mconcat = foldr mappend mempty

instance Monoid [a] where
  -- mempty :: [a]
  mempty = []

  -- mappend :: [a] -> [a] -> [a]
  mappend = (+)

instance Monoid a => Monoid (Maybe a) where
  -- mempty :: Maybe a
  mempty = Nothing

  -- mappend :: Maybe a -> Maybe a -> Maybe a
  Nothing   `mappend` my        = my
  mx        `mappend` Nothing   = mx
  Just x    `mappend` Just y    = Just (x `mappend` y)

instance Monoid Int where
  -- mempty :: Int
  mempty = 0

  -- mappend :: Int -> Int -> Int
  mappend = (+)

instance Monoid Int where
  -- mempty :: Int
  mempty = 1

  -- mappend :: Int -> Int -> Int
  mappend = (*)

newtype Sum a = Sum a
    deriving (Eq, Ord, Show, Read)

getSum :: Sum a -> a
getSum (Sum x) = x

instance Num a => Monoid (Sum a) where
  -- mempty :: Sum a
  mempty = Sum 0

  -- mappend :: Sum a -> Sum a -> Sum a
  (Sum x) `mappend` (Sum y) = Sum (x + y)

newtype Product a = Product a
    deriving (Eq, Ord, Show, Read)

getProduct :: Product a -> a
getProduct (Product x) = x

instance Num a => Monoid (Product a) where
  -- mempty :: Product a
  mempty = Product 1

  -- mappend :: Product a -> Product a -> Product a
  (Product x) `mappend` (Product y) = Product (x * y)

newtype All = All Bool
    deriving (Eq, Ord, Show, Read)

instance Monoid All where
  -- mempty :: All
  mempty = All True

  -- mappend :: All -> All -> All
  All x `mappend` All y = All (x && y)

newtype Any = Any Bool
    deriving (Eq, Ord, Show, Read)

instance Monoid Any where
  -- mempty :: Any
  mempty = Any False

  -- mappend :: Any -> Any -> Any
  Any x `mappend` Any y = Any (x || y)

--}
--
-- Section 4.2

fold :: Monoid a => [a] -> a
fold []     = mempty
fold (x:xs) = x `mappend` fold xs

data Tree a = Leaf a | Node (Tree a) (Tree a)
              deriving Show  

fold :: Monoid a => Tree a -> a
fold (Leaf x)   = x
fold (Node l r) = fold l `mappend` fold r

class Foldable t where
  fold :: Monoid a => t a -> a
  foldMap :: Monoid b => (a -> b) -> t a -> b
  foldr :: (a -> b -> b) -> b -> t a -> b
  foldl :: (a -> b -> a) -> a -> t b -> a 

instance Foldable [] where
  -- fold :: Monoid a => [a] -> a
  fold []       = mempty
  fold (x:xs)   = x `mappend` fold xs

  -- foldMap :: Monoid b => (a -> b) -> [a] -> b
  foldMap f []      = mempty
  foldMap f (x:xs)  = f x `mappend` foldMap f xs

  -- foldr :: (a -> b -> b) -> b -> [a] -> b
  foldr op v []     = v
  foldr op v (x:xs) = x `op` (foldr op xs v)

  -- foldl :: (a -> b -> a) -> a -> [b] -> a 
  foldl op v []     = v
  foldl op v (x:xs) = foldl op xs (v `op` x)

instance Foldable Tree where
  -- fold :: Monoid a => Tree -> a
  fold (Leaf x)         = x
  fold (Node l r)       = fold l `mappend` fold r

  -- foldMap :: Monoid b => (a -> b) -> t a -> b
  foldMap f (Leaf x)    = f x
  foldMap f (Node l r)  = fold l `mappend` fold r

  -- foldr :: (a -> b -> b) -> b -> ta -> b
  foldr op v (Leaf x)   = op x v
  foldr op v (Node l r) = foldr op (foldr op v r) l

  -- foldl :: (a -> b -> a) -> a -> tb -> a 
  foldl op v (Leaf x)   = op v x
  foldl op v (Node l r) = foldl op (foldl op v l) r
