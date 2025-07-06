-------------------------------------------------------
--  EXERCISE 14.2 - PROGRAMMING IN HASKELL - HUTTON  --
-------------------------------------------------------

-- Suppose B is a monoid and A is any set. Consider the set B^A of all
-- functions from A to B. We can turn the latter into a monoid by defining:
--  * The product of two functions f and g to be the function
--    \x -> f(x) *  g(x), where `*` denotes the product in B.
--  * The identity element to be the constant function \a -> e_B, where
--    e_B is the identity element of B.

instance Monoid b => Monoid (a -> b) where
    -- mempty :: a -> b
    mempty = \_ -> mempty

    -- mappend :: (a -> b) -> (a -> b) -> (a -> b)
    f `mappend` g = \x -> f x `mappend` g x
