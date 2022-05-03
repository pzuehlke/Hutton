------------------------------------------------------
--  Exercise 8.5 - Programming in Haskell - Hutton  --
------------------------------------------------------

data Expr = Val Int | Add Expr Expr

folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a
folde f _ (Val n)   = f n
folde f g (Add l r) = g (folde f g l) (folde f g r)
