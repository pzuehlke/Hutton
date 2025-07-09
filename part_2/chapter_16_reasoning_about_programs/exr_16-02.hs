-------------------------------------------------------
--  Exercise 16.2 - Programming in Haskell - Hutton  --
-------------------------------------------------------

data Nat = Zero | Succ Nat

add :: Nat -> Nat -> Nat
add Zero        m = m
add (Succ n)    m = Succ (add n m)

-- Lemma: add n Zero = n for all n.
--
-- Proof: By induction on n.
-- Base case:
--      add Zero Zero        {applying add}
--    = Zero
-- Inductive case:
--      add (Succ n) Zero    {applying add}
--    = Succ (add n Zero)    {induction hypothesis}
--    = Succ n               []
--
--
-- Theorem: add n m = add m n for all m, n >= 0.
--
-- Proof: By induction on n.
-- Base case:
--      add Zero m           {applying add}
--    = m                    {applying Lemma 1 with n = m}
--    = add m Zero
-- Inductive case:
--      add (Succ n) m       {applying add}
--    = Succ (add n m)       {induction hypothesis}
--    = Succ (add m n)       {Exercise 1, with m and n interchanged}
--    = add m (Succ n)       []
