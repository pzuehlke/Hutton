----------------------------------------------------
--  CHAPTER 17 - PROGRAMMING IN HASKELL - HUTTON  --
----------------------------------------------------

data Expr = Val Int | Add Expr Expr

eval :: Expr -> Int
eval (Val n)    = n
eval (Add x y)  = eval x + eval y

-- e1 = 1 + 2
e1 :: Expr
e1 = Add (Val 1) (Val 2)

type Stack = [Int]

eval' :: Expr -> Stack -> Stack
-- Specification: eval' e s = eval e : s
-- Case e = Val n:
--      eval' (Val n) s
--    = eval (Val n) : s
--    = n : s
-- Case e = Add x y
--      eval' (Add x y) s
--    = eval (Add x y) : s
--    = (eval x + eval y) : s
--    = add (eval y : eval x : s)       where add (m : n : s) = (n + m) : s
--    = add (eval y : (eval' x s))
--    = add (eval' y (eval' x s))
-- In conclusion, we have:
eval' (Val n) s     = push n s
eval' (Add x y) s   = add (eval' y (eval' x s))

push :: Int -> Stack -> Stack
push n s = n : s

add :: Stack -> Stack
add (m : n : s) = (n + m) : s

-- eval e = head (eval' e [])

-- Section 17.4: Adding a continuation

type Cont = Stack -> Stack
eval'' :: Expr -> Cont -> Cont
-- Specification: eval'' e c s = c (eval' e s)
-- Case e = Val n:
--      eval'' (Val n) c s
--    = c (eval' (Val n) s)
--    = c (push n s)
-- Case e = Add x y
--      eval'' (Add x y) c s
--    = c (eval' (Add x y) s)
--    = c (add (eval' y (eval' x s)))
--    = (c . add) (eval' y (eval' x s))
--    = eval'' y (c . add) (eval' x s)      Note that eval'' (c . add) :: Cont
--    = eval'' x (eval'' y (c . add)) s
-- In conclusion, we have:
eval'' (Val n)   c s    = c (push n s)
eval'' (Add x y) c s    = eval'' x (eval'' y (c . add)) s
-- Hence eval' e s = eval'' e id s

-- Section 17.5: Defunctionalising
haltC :: Cont
haltC = id

pushC :: Int -> Cont -> Cont
pushC n c = c . push n

addC :: Cont -> Cont
addC c = c . add

{-
eval' :: Expr -> Cont
eval' e = eval'' e haltC

eval'' :: Expr -> Cont -> Cont
eval'' (Val n)   c = pushC n c
eval'' (Add x y) c = eval'' x (eval'' y (addC c))
-}
data Code = HALT | PUSH Int Code | ADD Code deriving Show

{- 
HALT :: Code
PUSH :: Int -> Code -> Code
ADD :: Code -> Code

exec :: Code -> Cont
exec HALT       = haltC
exec (PUSH n c) = pushC n (exec c)
exec (ADD c)    = addC (exec c)

-- Case code = HALT:
--      exec HALT s
--    = haltC s
--    = id s
--    = s
-- Case code = PUSH n c
--      exec (PUSH n c) s
--    = pushC n (exec c) s
--    = (exec c . push n) s
--    = exec c (push n s)
--    = exec c (n : s)
-- Case code = ADD c
--      exec (ADD c) s
--    = addC (exec c) s
--    = (exec . add) s
--    = exec c (add s)
--    = exec c (add (m : n : s'))    where s = (m : n : s')
--    = exec c (n + m : s')
-}

exec :: Code -> Stack -> Stack
exec HALT       s           = s
exec (PUSH n c) s           = exec c (n : s)
exec (ADD c)    (m : n : s) = exec c (n + m : s)

comp :: Expr -> Code
comp e = comp' e HALT

comp' :: Expr -> Code -> Code
comp' (Val n)   c = PUSH n c
comp' (Add x y) c = comp' x (comp' y (ADD c))

-- Correctness equations:
-- exec (comp e) s = eval : s
-- exec (comp' e c) s = exec c (eval e : s)
