----------------------------------------------------
--  Chapter 14 - Programming in Haskell - Hutton  --
----------------------------------------------------

-- Section 14.1
class Monoid a where
    mempty :: a
    mappend :: a -> a -> a

    mconcat :: [a] -> [a]
    mconcat = foldr mappend mempty

[x, y, z] == x `mappend` (y `mappend` (z `mappend` mempty))

-- Monoid laws:
mempty `mappend` x          = x
x `mappend` mempty          = x
x `mappend` (y `mappend` z) = (x `mappend` y) `mappend` z

-- Examples
instance Monoid [a] where
    -- mempty :: [a]
    mempty = []

    -- mappend :: [a] -> [a] -> [a]
    mappend = (++)

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

newtype Sum a = Sum a deriving (Eq, Ord, Show, Read)

getSum :: Sum a -> a
getSum (Sum x) = x

instance Num a => Monoid (Sum a) where
    -- mempty :: Sum a
    mempty = Sum 0

    -- mappend :: Sum a -> Sum a -> Sum a
    Sum x `mappend` Sum y = Sum (x + y)

newtype Product a = Product a deriving (Eq, Ord, Show, Read)

getProduct :: Product a -> a
getProduct (Product x) = x

instance Product a => Monoid (Product a) where
    -- mempty :: Product a
    mempty = 1

    -- mappend :: Product a -> Product a -> Product a
    Product x `mappend` Product y = Product (x * y)

mconcat [All True, All True, All True] == All True
mconcat [Any False, Any False, Any False] == Any False

-- Section 14.2
fold :: Monoid a => [a] -> a
fold []     = mempty
fold (x:xs) = x `mappend` fold xs

data Tree a = Leaf a | Node (Tree a) (Tree a) deriving Show
fold :: Monoid a => Tree a -> a
fold (Leaf x)   = x
fold (Node l r) = fold l `mappend` fold r

class Foldable t where
    fold :: Monoid a => t a -> a
    foldMap :: Monoid b => (a -> b) -> t a -> t b
    foldr :: (a -> b -> b) -> b -> t a -> b
    foldl :: (a -> b -> a) -> a -> t b -> a

-- Examples:
instance Foldable [] where
    -- fold :: Monoid a => [a] -> a
    fold []     = mempty
    fold (x:xs) = x `mappend` fold xs

    -- foldMap :: Monoid b => (a -> b) -> [a] -> b
    foldMap _ []        = mempty
    foldMap f (x:xs)    = f x `mappend` (foldMap f xs)

    -- foldr :: (a -> b -> b) -> b -> [a] -> b
    foldr _ v []        = v
    foldr f v (x:xs)    = f x (foldr f v xs)

    -- foldl :: (a -> b -> a) -> a -> [b] -> a
    foldl _ v []        = v
    foldl f v (y:ys)    = foldl f (f v y) ys

instance Foldable Tree where
    -- fold :: Monoid a => Tree a -> a
    fold (Leaf x)   = x
    fold (Node l r) = (fold l) `mappend` fold r

    -- foldMap :: Monoid b => (a -> b) -> Tree a -> b
    foldMap f (Leaf x)      = f x
    foldMap f (Tree l r)    = (foldMap f l) `append` (foldMap f r)

    -- foldr :: (a -> b -> b) -> b -> Tree a -> b
    foldr f v (Leaf x)      = f x v
    foldr f v (Tree l r)    = foldr f (foldr f v r) l

    -- foldl :: (a -> b -> a) -> a -> Tree b -> a
    foldl f v (Leaf y)      = f v y
    foldl f v (Tree l r)    = foldl f (foldl f v l) r

null    :: t a -> Bool
length  :: t a -> Int
elem    :: Eq a => a -> t a -> Bool
maximum :: Ord a => t a -> a
minimum :: Ord a => t a -> a
sum     :: Num a => t a -> a
product :: Num a => t a -> a

foldr1 :: (a -> a -> a) -> t a -> a
foldl1 :: (a -> a -> a) -> t a -> a

toList :: t a -> [a]

foldr f v = foldr f v . toList
foldl f v = foldl f v . toList
foldr1 f  = foldr1 f  . toList
foldl1 f  = foldl1 f  . toList

null    = null    . toList
length  = length  . toList
elem x  = elem x  . toList
maximum = maximum . toList
minimum = minimum . toList
sum     = sum     . toList
product = product . toList

fold        = foldMap id
foldMap f   = foldr (mappend . f) mempty
toList      = foldMap (\x -> [x])

-- Generic functions
average :: Foldable t => t Int -> Int
average ns = sum ns `div` length ns

and :: Foldable t => t Bool -> Bool
and = getAll . foldMap All

or :: Foldable t => t Bool -> Bool
or = getAny . foldMap Any

all :: Foldable t => (a -> Bool) -> t a -> Bool
all p = getAll . foldMap (All . p)

-- Section 14.3
class Functor F where
    fmap :: (a -> b) -> F a -> F b

map :: (a -> b) -> [a] -> [b]
map g []        = []
map g (x:xs)    = g x : (map g xs)

traverse :: (a -> Maybe b) -> [a] -> Maybe b
traverse g []       = pure []
traverse g (x:xs)   = pure (:) <*> g x <*> traverse g xs

dec :: Int -> Maybe Int
dec n = if n > 0 then Just (n - 1) else Nothing

class (Functor t, Foldable t) => Traversable t where
    traverse :: Applicative f => (a ->> f b) -> t a -> f (t b)

-- Examples

instance Traversable [] where
    -- traverse :: Applicative f => (a -> f b) -> [a] -> f [b]
    traverse g []       = pure []
    traverse g (x:xs)   = pure (:) <*> g x <*> traverse g xs

instance Traversable Tree where
    -- traverse :: Applicative f => (a -> f b) -> Tree a -> f (Tree b)
    traverse g (Leaf x)     = pure Leaf <*> g x
    traverse g (Node l r)   = pure Node <*> traverse g l <*> traverse g r

sequenceA :: Applicative f => t (f a) -> f (t a )
sequenceA = traverse id

-- traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
traverse g = sequenceA . fmap g

mapM :: Monad m => (a -> m b) -> t a -> m (t b)
sequence :: Monad m => t ( m a) -> m (t a)
