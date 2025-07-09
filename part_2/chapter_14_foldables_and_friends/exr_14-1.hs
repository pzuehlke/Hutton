-------------------------------------------------------
--  EXERCISE 14.1 - PROGRAMMING IN HASKELL - HUTTON  --
-------------------------------------------------------

-- The idea is that if A and B are monoids, then their cartesian product A x B
-- becomes a monoid by defining its identity element to be (e_A, e_B) and
-- with the "product" given coordinate-wise (e_X is the identity element of X).

instance (Monoid a, Monoid b) => Monoid (a, b) where
    -- mempty :: (a, b)
    mempty = (mempty, mempty)

    -- mappend :: (a, b) -> (a, b) -> (a, b)
    (x1, y1) `mappend` (x2, y2) = (x1 `mappend` x2, y1 `mappend` y2)
