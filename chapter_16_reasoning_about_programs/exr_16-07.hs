-------------------------------------------------------
--  Exercise 16.7 - Programming in Haskell - Hutton  --
-------------------------------------------------------

data Maybe' a = Just' a | Nothing'

instance Functor Maybe' where
  -- fmap :: (a -> b) -> Maybe a -> Maybe b
  fmap _ Nothing'    = Nothing'
  fmap f (Just' x)   = Just' (f x)

-- Theorem: (i)     fmap id = id;
--          (ii)    fmap (g . f) = (fmap g) . (fmap f).
--
-- Proof:
-- (i) By case analysis:
--      fmap id Nothing = {applying fmap} Nothing
--
--      fmap id Just x  = {applying fmap} Just (id x)
--                      = {applying id} Just x
--
-- (ii) Also by case analysis:
--      fmap (g . f) Nothing    = {applying fmap} Nothing
--
--      fmap (g . f) Just x     = {applying fmap} (Just (g . f) x)
--                              = {applying .} (Just (g (f x)))
--                              = {unapplying fmap} fmap g (Just (f x))
--                              = {unapplying fmap} fmap g (fmap f (Just x))
--                              = {unapplying .} ((fmap g) . (fmap f)) (Just x)
