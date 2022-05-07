-------------------------------------------------------
--  Exercise 16.3 - Programming in Haskell - Hutton  --
-------------------------------------------------------

myAll :: (a -> Bool) -> [a] -> Bool
myAll p []      = True
myAll p (x:xs)  = p x && myAll p xs

myReplicate :: Int -> a -> [a]
myReplicate 0 _ = []
myReplicate n x = x : (myReplicate (n - 1) x)

-- Theorem: all (== x) (replicate n x) = True.
--
-- Proof: By induction on n.
-- Base case:
-- all (== x) (replicate 0 x)       = {applying replicate} all (== x) []
--                                  = {applying all} True
-- Inductive case:
-- all (== x) (replicate (n + 1) x)
--      = {applying replicate} all (== x) (x : (replicate n x))
--      = {applying all} ((== x) x) && (all (== x) (replicate n x))
--      = {evaluating} True && (all (== x) (replicate n x)) 
--      = {induction hypothesis} True && True
--      = {evaluating} True                                                 []
