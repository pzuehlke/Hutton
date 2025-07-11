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

-- fmap0 :: a -> F a
-- fmap1 :: (a -> b) -> F a -> F b
-- fmap2 :: (a -> b -> c) -> F a -> F b -> F c
-- fmap3 :: (a -> b -> c -> d) -> F a -> F b -> F c -> F d
--   .
--   .
--   .

pure :: a -> F a
(<*>) :: F (a -> b) -> F a -> F b

-- pure g <*> x1 <*> x2 <*> ... <*> xn
-- fmap0 = pure
-- fmap1 g x = pure g <*> x
-- fmap2 g x y = pure g <*> x <*> y
-- fmap3 g x y z = pure g <*> x <*> y <*> z
--   .
--   .
--   .

class Functor F => Applicative F where
    pure :: a -> F a
    (<*>) :: F (a -> b) -> F a -> F b

-- Examples

instance Applicative Maybe where
    -- pure :: a -> Maybe a
    pure x = Just x
    -- (<*>) :: Maybe (a -> b) -> Maybe a -> Maybe b
    Nothing  <*> _  = Nothing
    (Just g) <*> mx = fmap g mx

instance Applicative  [] where
    -- pure :: a -> [a]
    pure x = [x]
    -- (<*>) :: [a -> b] -> [a] -> [b]
    gs <*> xs = [g x | g <- gs, x <- xs]

prods :: [Int] -> [Int] -> [Int]
prods xs ys = pure (*) <*> xs <*> ys

instance Applicative IO where
    -- pure :: a -> IO a
    pure = return
    -- (<*>) :: IO (a -> b) -> IO a -> IO b
    mg <*> mx = do {g <- mg; x <- mx; return (g x)}

getChars :: Int -> IO String
getChars 0 = return []
getChars n = pure (:) <*> getChar <*> getChars (n - 1)

sequenceA :: Applicative F => [F a] -> F [a]
sequneceA []        = pure []
sequenceA (mx:mxs)    = pure (:) <*> mx <*> sequenceA mxs

getChars :: Int -> IO String
getChars n = sequenceA (replicate n getChar)

-- Applicative laws
pure id <*> x   = x
pure (g x)      = pure g <*> pure x
x <*> pure y    = pure (\g -> g y) <*> x
x <*> (y <*> z) = (pure (.) <*> x <*> y) <*> z
    
-- pure :: a -> F a
-- (<*>) :: F (a -> b) -> F a -> F b

-- (a) pure id <*> x   = x
id :: a -> a
pure id :: F (a -> a)
x :: F a                -- right side has type F a
pure id <*> x :: F a    -- left side has type F a

-- (b) pure (g x) = pure g <*> pure x
x :: a
g :: a -> b
g x :: b
pure (g x) :: F b   -- left side has type F b
pure g = F (a -> b)
pure x :: F a
pure g <*> pure x   -- right side has type F b

-- (c) f <*> pure y = pure (\g -> g y) <*> f
f :: F (a -> b)
y :: a
pure y :: F a
f <*> pure y :: F b             -- left side has type F b
\g -> g y :: (a -> b) -> b
pure (\g -> g y) = F ((a -> b) -> b)
pure (\g -> g y) <*> f :: F b   -- right side has type F b

-- (d) h <*> (g <*> x) = (pure (.) <*> h <*> g) <*> x
(.) :: (b -> c) -> (a -> b) -> (a -> c)
pure (.) :: F [(b -> c) -> (a -> b) -> (a -> c)]
h :: F (b -> c)
g :: F (a -> b)
x :: F a
g <*> x :: F b
h <*> (g <*> x) :: F c              -- left side has type F c
pure (.) <*> h :: F [(a -> b) -> (a -> c)
pure (.) <*> h <*> g :: F (a -> c)
(pure (.) <*> h <*> g) <*> x :: F c -- right side has type F c



-- SECTION 12.3




