-------------------------------------------------------
--  Exercise 16.5 - Programming in Haskell - Hutton  --
-------------------------------------------------------

myTake :: Int -> [a] -> [a]
myTake 0 _          = []
myTake _ []         = []
myTake n (x:xs)     = x : take (n - 1) xs

myDrop :: Int -> [a] -> [a]
myDrop 0 xs     = xs
myDrop _ []     = []
myDrop n (_:xs) = myDrop (n - 1) xs

-- Theorem: take n xs ++ drop n xs == xs.
--
-- Proof: By induction on n.
-- Base case:
-- take 0 xs ++ drop 0 xs   = {applying take, drop} [] ++ xs
--                          = {applying ++} xs
--
-- Inductive case:
-- If length(xs) = 0, then induction is not required, since for any n > 0:
-- take n [] ++ drop n []   = {applying take, drop} [] ++ []
--                          = {applying ++} []
-- Otherwise:
-- take (n + 1) (x:xs) ++ drop (n + 1) (x:xs)
--      = {applying take, drop} (x : take n xs) ++ (drop n xs)
--      = {applying ++} x : ((take n xs) ++ (drop n xs))
--      = {induction hypothesis} x : xs                                     []
