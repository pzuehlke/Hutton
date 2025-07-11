------------------------------------------------------
--  Exercise 12.2 - Programming in Haskell - Hutton  --
------------------------------------------------------

-- Note that (->) is an infix operator, so (->) a applies -> to its first
-- argument a. Hence, using more suggestive notation, ((->) a) means a -> *.
instance Functor ((->) a) where
    -- fmap :: (b -> c) -> (a -> b) -> (a -> c)
    fmap g f = g . f

-- Verification of the functor laws:
-- fmap id f = id . f = f   for all f
-- Now let
-- h :: c -> d
-- g :: b -> c
-- We need to show that fmap (h . g) = (fmap h) . (fmap g). Indeed:
-- fmap (h . g) f = (h . g) . f
-- [(fmap h) . (fmap g)] f  = fmap h (fmap g f)
--                          = fmap h (g . f)
--                          = h . (g . f)
-- Since composition is associative, the second law holds.
