------------------------------------------------------
--  Exercise 8.8 - Programming in Haskell - Hutton  --
------------------------------------------------------
-- Contains a model for the propositional calculus and a function which checks
-- whether a proposition is a tautology.

-- | Recursive data type which models the propositional calculus:
data Prop   = Const Bool
            | Var Char
            | Not Prop
            | And Prop Prop
            | Or Prop Prop
            | Implies Prop Prop
            | Equiv Prop Prop

-- | Dictionaries:
type Assoc k v = [(k, v)]

-- | Dictionaries relating variable names to booleans:
type Subst = Assoc Char Bool


-- Some sample propositions:
p1 :: Prop
-- A and (~A)
p1 = And (Var 'A') (Not (Var 'A'))

p2 :: Prop
-- (A and B) -> A
p2 = Implies (And (Var 'A') (Var 'B')) (Var 'A')

p3 :: Prop
-- A -> (A and B)
p3 = Implies (Var 'A') (And (Var 'A') (Var 'B'))

p4 :: Prop
-- (A and (A -> B)) -> B
p4 = Implies (And (Var 'A') (Implies (Var 'A') (Var 'B'))) (Var 'B')

p5 :: Prop
-- A <-> B implies (~A) <-> (~B)
p5 = Implies (Equiv (Var 'A') (Var 'B')) (Equiv (Not (Var 'A')) (Not (Var 'B')))


-- | Returns the first occurrence of a value associated to a given key in a dict.
find :: Eq k => k -> Assoc k v -> v
find k dict = head [v | (k', v) <- dict, k' == k]

-- | Removes all duplicates from a list; return a new list.
removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates []     = []
removeDuplicates (x:xs) = x : removeDuplicates [y | y <- xs, y /= x]

-- | Given a dictionary associating True or False (independently) to each
-- variable occurring in a proposition, evaluates the latter by substituting
-- these values.
eval :: Subst -> Prop -> Bool
eval _ (Const b)        = b
eval s (Var x)          = find x s
eval s (Not p)          = not (eval s p)
eval s (And p q)        = eval s p && eval s q
eval s (Or p q)         = eval s p || eval s q
-- To avoid using the ordering relation between False and True:
eval s (Implies p q)    = not (eval s p) || eval s q
eval s (Equiv p q)      = eval s p == eval s q

-- | Returns a list of all variables occurring in a given proposition P; each
-- variable is present in the list as many times as it occurs in P.
vars :: Prop -> [Char]
vars (Const _)      = []
vars (Var x)        = [x]
vars (Not p)        = vars p
vars (And p q)      = vars p ++ vars q
vars (Implies p q)  = vars p ++ vars q
vars (Equiv p q)    = vars p ++ vars q

-- | Returns a list of all variables occurring in a given proposition; each
-- such variable appears exactly once in the list.
uniqueVars :: Prop -> [Char]
uniqueVars = removeDuplicates . vars

-- | Given an integer n, generates a list consisting of all 2^n possible lists
-- of length n using the values False and True; these lists are ordered
-- lexicographically.
booleans :: Int -> [[Bool]]
booleans n     | n <= 0    = [[]]
booleans n     | otherwise = map (False:) bss ++ map (True:) bss
                           where
                             bss = bools (n - 1)

-- | Given a proposition involving n variables, returns a list of all 2^n
-- possible dictionaries associating to each of these variables one of the
-- values False or True (independently).
getSubstitutions :: Prop -> [Subst]
getSubstitutions p = map (zip vs) bss
           where
             vs = uniqueVars p
             bss = bools (length vs)

-- | Main function: decides whether a given proposition is a tautology.
isTautology :: Prop -> Bool
isTautology p = and [eval s p | s <- getSubstitutions p]
