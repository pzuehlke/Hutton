-------------------------------------------------------
--  Exercise 16.10 - Programming in Haskell - Hutton  --
-------------------------------------------------------

instance Monad [] where
    -- (>>=) :: [a] -> (a -> [b]) -> [b]
    xs >>= f = [y | x <- xs, y <- f x]

-- Theorem: The monad laws hold for []:
-- (i)     return x >>= f   = f x
-- (ii)    xs >>= return    = xs
-- (iii)   (xs >>= f) >>= g = xs >>= (\x -> (f x >>= g))

-- Proof:
--
-- (i)   We need to prove that
--          return x >>= f = f x
--
--       return x >>= f              {applying definition of return (= pure)}
--     = pure x >>= f                {applying definition of pure}
--     = [x] >>= f                   {applying >>=}
--     = [y | x <- [x], y <- f x]    {using definition of list comprehension}
--     = [y | y <- f x]              {using definition of list comprehension}
--     = f x                         []
--
-- (ii)  We need to prove that
--          xs >>= return = xs
--       xs >>= return                   {applying >>=}
--     = [y | x <- xs, y <- return x]    {applying definition of return}
--     = [y | x <- xs, y <- pure x]      {applying definition of pure}
--     = [y | x <- xs, y <- [x]]         {using prop. of list comprehension}
--     = [x | x <- xs]                   {using defn. of list comprehension}
--     = xs                              []
--
-- (iii) We need to prove that
--          (xs >>= f) >>= g = xs >>= (\x -> (f x >>= g))
--       The crucial step lies in noting that nested list comprehensions can be
--       flattened (and vice-versa). To abbreviate the notation, let h denote
--       the lambda function on the right side. 
--
--       xs >>= (\x -> (f x >>= g))         {applying definition of >>=}
--     = [y | x <- xs, y <- h x]            {applying definition of h}
--     = [y | x <- xs, y <- (f x >>= g)]    {applying >>=}
--     = [y | x <- xs, y <- [z | w <- f x, z <- g w]]   {prop. of list compr.}
--     = [z | x <- xs, w <- f x, z <- g w]              {prop. of list compr.}
--     = [z | w <- [y | x <- xs, y <- f x], z <- g w]   {unapplying >>=}
--     = [z | w <- (xs >>= f), z <- g w]                {unapplying >>=}
--       (xs >>= f) >>= g                               []
