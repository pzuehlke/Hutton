----------------------------------------------------
--  CHAPTER 17 - PROGRAMMING IN HASKELL - HUTTON  --
----------------------------------------------------
-- This is a minimal working script that does all the work of the
-- compiler/virtual machine built in the chapter. I wrote this to clarify
-- what else needed to be done in Exercise 17-1.

type Stack = [Int]

data Expr = Val Int | Add Expr Expr deriving Show

data Code = HALT | PUSH Int Code | ADD Code

eval :: Expr -> Int
eval (Val n)    = n
eval (Add x y)  = eval x + eval y

exec :: Code -> Stack -> Stack
exec HALT       s           = s
exec (PUSH n c) s           = exec c (n : s)
exec (ADD c)    (m : n : s) = exec c (n + m : s)

comp' :: Expr -> Code -> Code
comp' (Val n)   c = PUSH n c
comp' (Add x y) c = comp' x (comp' y (ADD c))

comp :: Expr -> Code
comp e = comp' e HALT

-- Testing:
e :: Expr  -- 2 + (3 + 4)
e = Add (Val 2) (Add (Val 3) (Val 4))
