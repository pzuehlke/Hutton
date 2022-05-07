-------------------------------------------------------
--  Exercise 16.2 - Programming in Haskell - Hutton  --
-------------------------------------------------------

data Nat = Zero | Succ Nat

add :: Nat -> Nat -> Nat
add Zero        m = m
add (Succ n)    m = Succ (add n m)

-- Lemma 1: add n Zero = n for all n, by induction on n.
--
-- Proof: By induction on n.
-- Base case:
-- add Zero Zero        = {applying add} Zero
-- Inductive case:
-- add (Succ n) Zero    = {applying add} Succ (add n Zero)
--                      = {induction hypothesis} Succ n                     []
--
--
-- Lemma 2: add (Succ n) m = add n (Succ m)
--
-- Proof: By induction on n.
-- Base case:
-- add (Succ Zero) m        = {applying add} Succ (add Zero m)
--                          = {applying add} Succ m
--                          = {unapplying add} add Zero (Succ m)
-- Inductive case:
-- add (Succ (Succ n)) m    = {applying add} Succ (add (Succ n) m)
--                          = {inductive hypothesis} Succ (add n (Succ m))
--                          = {unapplying add} add (Succ n) (Succ m)        []
--
--
-- Theorem: add n m = add m n for all m, n >= 0.
--
-- Proof: By induction on n.
-- Base case:
-- add Zero m       = {applying add} m
--                  = {applying Lemma 1 with n = m} add m Zero
-- Inductive case:
-- add (Succ n) m   = {applying add} Succ (add n m)
--                  = {induction hypothesis} Succ (add m n)
--                  = {unapplying add} add (Succ m) n
--                  = {Lemma 2} add m (Succ n)                              []
