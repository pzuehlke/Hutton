------------------------------------------------------
--  Exercise 12.3 - Programming in Haskell - Hutton  --
------------------------------------------------------

-- It suffices to think about the types that `pure` and `<*>` must have. There
-- is only one natural choice in each case.

instance Applicative ((->) a) where
    -- pure :: b -> (a -> b)
    -- Given y in B, we must produce a function A -> B:
    pure y = \_ -> y

    -- (<*>) :: (a -> (b -> c)) -> (a -> b) -> (a -> c)
    -- Given a function g : A x B -> C of two variables and a function
    -- h : A -> B, we must produce a function A -> C:
    g <*> h = \x -> g x (h x)
