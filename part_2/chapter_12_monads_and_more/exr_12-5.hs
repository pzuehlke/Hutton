------------------------------------------------------
--  Exercise 12.5 - Programming in Haskell - Hutton  --
------------------------------------------------------

-- F is an applicative functor throughout.

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

-- (c) h <*> pure y = pure (\g -> g y) <*> h
h :: F (a -> b)
y :: a
pure y :: F a
h <*> pure y :: F b             -- left side has type F b
\g -> g y :: (a -> b) -> b
pure (\g -> g y) = F ((a -> b) -> b)
pure (\g -> g y) <*> h :: F b   -- right side has type F b

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
