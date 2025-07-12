------------------------------------------------------
--  Exercise 12.8 - Programming in Haskell - Hutton  --
------------------------------------------------------

type State = Int
newtype ST a = S (State -> (a, State))

app :: ST a -> State -> (a, State)
app (S st) s = st s

-- To solve this exercise, it helps to compare the diagrams on pp. 169-170.

instance Functor ST where
    -- fmap :: (a -> b) -> ST a -> ST b
    fmap g st = do
        x <- st     -- perform stateful computation
        return g x  -- return g of the output value

instance Applicative ST where
    -- pure :: a -> ST a
    pure x = S (\s -> (x, s))

    -- <*> :: ST (a -> b) -> ST a -> ST b
    stf <*> stx = do
        f <- stf
        x <- stx
        return f x

instance Monad ST where
    -- (>>=) :: ST a -> (a -> ST b) -> ST b
    st >>= f = S (\s -> let (x, s') = app st s in app (f x) s')
