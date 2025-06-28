------------------------
--  Abstract Machine  --
------------------------

data Expr = Val Int | Add Expr Expr | Mul Expr Expr
data Op = EVALA Expr | EVALM Expr | ADD Int | MUL Int
type Cont = [Op]


-- Sample expressions:
e1 :: Expr
-- (1 + 2) + 3 = 6
e1 = Add (Add (Val 1) (Val 2)) (Val 3)

e2 :: Expr
-- 2 + (3 + 4) = 9
e2 = Add (Val 2) (Add (Val 2) (Val 4))

e3 :: Expr
-- 1 + (2 * 3) = 7
e3 = Add (Val 1) (Mul (Val 2) (Val 3))

e4 :: Expr
-- (1 + 2) * 3 = 9
e4 = Mul (Add (Val 1) (Val 2)) (Val 3)


eval :: Expr -> Cont -> Int
eval (Val n)   c = exec c n
eval (Add x y) c = eval x (EVALA y : c)
eval (Mul x y) c = eval x (EVALM y : c)

exec :: Cont -> Int -> Int
exec []           n = n
exec (EVALA y : c)  n   = eval y (ADD n : c)
exec (EVALM y : c)  n   = eval y (MUL n : c)
exec (ADD m : c)   n    = exec c (n + m)
exec (MUL m : c)   n    = exec c (m * n)

getValue :: Expr -> Int
getValue e = eval e []
