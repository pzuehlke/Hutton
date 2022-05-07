------------------------------------------------------
--  Exercise 8.7 - Programming in Haskell - Hutton  --
------------------------------------------------------

-- data Maybe a = Nothing | Just a


-- Part (a)
instance Eq a => Eq (Maybe a) where
  (==), (/=) :: Maybe a -> Maybe a -> Bool

  Nothing   == Nothing      = True
  (Just x)  == (Just y)     = x == y
  _         == _            = False


-- Part (b)
instance Eq a => Eq [a] where
  (==), (/=) :: [a] -> [a] -> Bool

  []     == []      = True
  (x:xs) == (y:ys)  = (x == y) && (xs == ys)
  _      == _       = False
