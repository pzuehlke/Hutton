---------------------------------------------------
--  CHAPTER 12 - PROGRAMMING IN HASKELL - HUTTON  --
---------------------------------------------------

-- SECTION 12.1
inc :: [Int] -> [Int]
inc []      = []
inc (n:ns)  = (n + 1) : inc ns

sqr :: [Int] -> [Int]
sqr []      = []
sqr (n:ns)  = (n + 1) : sqr ns

map :: (a -> b) -> [a] -> [b]
map f   []      = []
map f (x:xs)    = (f x) : map f xs

inc = map (+1)
sqr = map (^2)

class functor F where
    fmap :: (a -> b) -> F a -> F b

instance Functor [] where
    -- fmap :: (a -> b) -> [a] -> [b]
fmap = map

data Maybe a = Nothing | Just a

instance Functor Maybe where
    -- fmap :: (a -> b) -> Maybe a -> Maybe b
fmap _ Nothing  = Nothing
fmap g (Just x) = Just (g x)

data Tree a = Leaf a | Node (Tree a) (Tree a) deriving Show

instance Functor Tree where
    -- fmap :: (a -> b) -> Tree a -> Tree b
fmap g (Leaf x)     = Leaf (f x)
fmap g (Node l r)   = Node (fmap g l) (fmap g r)

instance Functor IO where
    -- fmap :: (a -> b) -> IO a -> IO b
fmap g mx = do
    x <- mx
    return (g x)

inc :: Functor F => F Int -> F Int
inc = fmap (+1)

-- SECTION 12.2



