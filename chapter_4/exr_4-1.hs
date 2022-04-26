------------------------------------------------------
--  EXERCISE 4.1 - PROGRAMMING IN HASKELL - HUTTON  --
------------------------------------------------------

halve :: [a] -> ([a], [a])
halve xs = (take m xs, drop m xs)
  where
      m = (length xs) `div` 2
