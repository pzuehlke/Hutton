------------------------------------------------------
--  Exercise 8.1 - Programming in Haskell - Hutton  --
------------------------------------------------------

data Nat = Zero | Succ Nat deriving Show

add :: Nat -> Nat -> Nat
add Zero n      = n
add (Succ m) n  = Succ (add m n)

mult :: Nat -> Nat -> Nat
mult Zero n     = Zero
mult (Succ m) n = add (mult m n) n

natToInt :: Nat -> Int
natToInt Zero       = 0
natToInt (Succ n)   = natToInt n + 1

-- Sample test:  3 x 2 = 6 ?
-- natToInt (mult (Succ (Succ (Succ Zero))) (Succ (Succ Zero))) == 6
