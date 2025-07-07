-------------------------------------------------------
--  EXERCISE 14.5 - PROGRAMMING IN HASKELL - HUTTON  --
-------------------------------------------------------

-- Same idea as on pp. 203 and 204 to define other higher-order functions:
-- first we convert to a list, and then we use the list version of filter:
filterF :: Foldable t => (a -> Bool) -> t a -> [a]
filterF p = filter p . toList

