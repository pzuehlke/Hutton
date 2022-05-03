------------------------------------------------------
--  Exercise 8.6 - Programming in Haskell - Hutton  --
------------------------------------------------------

data Expr = Val Int | Add Expr Expr

folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a
folde f _ (Val n)   = f n
folde f g (Add l r) = g (folde f g l) (folde f g r)

eval :: Expr -> Int
eval = folde id (+)

-- Sample test: 2 + 3 == 5?
-- eval (Add (Val 2) (Val 3))

size :: Expr -> Int
size = folde (\_ -> 1) (+)
-- Sample test: size (2 + (3 + (4 + 5))) == 4?
-- size (Add (Val 2) (Add (Val 3) (Add (Val 4) (Val 5))))
