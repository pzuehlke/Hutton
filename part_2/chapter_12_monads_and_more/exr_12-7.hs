------------------------------------------------------
--  Exercise 12.7 - Programming in Haskell - Hutton  --
------------------------------------------------------

data Expr a = Var a | Val Int | Add (Expr a) (Expr a) deriving Show
-- Think of expressions as arithmetic expressions involving integers and
-- substitutable variables, with addition as the only possible operation.

instance Functor Expr where
    -- fmap :: (a -> b) -> Expr a -> Expr b
    fmap f (Var x)      = Var (f x)
    fmap _ (Val n)      = Val n
    fmap f (Add l r)    = Add (fmap f l) (fmap f r)

instance Applicative Expr where
    -- pure :: a -> Expr a
    pure x = Var x

    -- (<*>) :: Expr (a -> b) -> Expr a -> Expr b
    -- Unfortunately, there are 3 x 3 = 9 cases to consider.
    -- First with (Var f) as the first argument. This one is natural:
    (Var f) <*> (Var x)     = Var (f x)
    (Var f) <*> (Val n)     = Val n
    (Var f) <*> (Add l r)   = Add (pure f <*> l) (pure f <*> r)
    -- Now with (Val m) as the first argument:
    (Val m) <*> _ = Val m
    -- What else could the result be? Returning _ wouldn't be type-correct
    -- in general, except in case _ = Val n for some n. And setting
    -- (Val m) <*> -- _ = Val 0 is also unnatural since we are working at a
    -- level where we don't need to know that + over Int has a neutral element.
    --
    -- And finally, with something of the form `Add` as the 1st argument.
    -- The intuition is that (g + h)(x) = g(x) + h(x) (where x is arbitrary):
    (Add eg eh) <*> _ = Add (eg <*> _) (eh <*> _)

instance Monad Expr where
    -- ...
