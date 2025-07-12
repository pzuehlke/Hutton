------------------------------------------------------
--  Exercise 12.6 - Programming in Haskell - Hutton  --
------------------------------------------------------

instance Monad ((->) a) where
    -- (>>=) :: (a -> b) -> (b -> (a -> c)) ->  (a -> c)
    -- Given functions f : A -> B of one variable and g : B x A -> C of two
    -- variables, we must produce a function A -> C:
    f >>= g = \x -> g (f x) x

-- Note the similarity to the definition of <*> in exercise 12-3.
