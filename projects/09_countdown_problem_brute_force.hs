----------------------------------------------------------
--  The Countdown Problem Solver - Brute Force Approach --
----------------------------------------------------------

data Op = Add | Sub | Mul | Div

instance Show Op where
    show Add = "+"
    show Sub = "-"
    show Mul = "*"
    show Div = "/"

-- | Recursive type which models a numeric expression.
data Expr = Val Int | App Op Expr Expr

instance Show Expr where
    show (Val n)      = show n
    show (App o l r)  = bracket l ++ " " ++ show o ++ " " ++ bracket r
                        where
                            bracket (Val n)    = show n
                            bracket e          = "(" ++ show e ++ ")"

-- List containing all members of Op:
ops :: [Op]
ops = [Add, Sub, Mul, Div]

-- | Checks if the application of an elementary operation to two integers is
-- valid according to the rules of the game.
valid :: Op -> Int -> Int -> Bool
valid Add _ _   = True 
valid Sub m n   = m > n 
valid Mul _ _   = True
valid Div m n   = m `mod` n == 0

-- | Applies an elementary operation to two integers.
apply :: Op -> Int -> Int -> Int
apply Add m n   = m + n
apply Sub m n   = m - n
apply Mul m n   = m * n
apply Div m n   = m `div` n

-- Some sample expressions:
-- (1 + 50) * (25 - 10) == 765
e :: Expr
e = App Mul (App Add (Val 1) (Val 50)) (App Sub (Val 25) (Val 10))

-- 1 + (2 * 3) == 7
e1 :: Expr
e1 = App Add (Val 1) (App Mul (Val 2) (Val 3))

-- (4 / 2) - 1 == 1
e2 :: Expr
e2 = App Sub (App Div (Val 4) (Val 2)) (Val 1)

-- 2 - 3 == invalid
e3 :: Expr
e3 = App Sub (Val 2) (Val 3)

-- (2 / 3) * 3 == invalid
e4 :: Expr
e4 = App Mul (App Div (Val 2) (Val 3)) (Val 3)

-- (2 * 3) / 3 == 2
e5 :: Expr
e5 = App Div (App Mul (Val 2) (Val 3)) (Val 3) 

-- | Returns a list of all the integers appearing in a given expression, prior
-- to its evaluation (and to the evaluation of any subexpressions).
values :: Expr -> [Int]
values (Val n)      = [n]
values (App _ l r)  = values l ++ values r

-- | Evaluates a given expression. Returns a singleton list whose only entry
-- is the result of the evaluation if the operation is valid, or an empty list
-- if the operation is invalid according to the rules of the game.
eval :: Expr -> [Int]
eval (Val n)        = [n | n > 0]
eval (App o l r)    = [apply o x y | x <- eval l, y <- eval r, valid o x y]

-- | Given a list, returns all possible sublists which can be formed from it by
-- deleting some of the elements while preserving the original order.
sublists :: [a] -> [[a]]
sublists []         = [[]]
sublists (x:xs)     = (sublists xs) ++ (map (x:) (sublists xs))

-- | Given x and and a list xs, returns a list of lists comprising all possible
-- ways to obtain a new list by inserting x into xs.
interleave :: a -> [a] -> [[a]]
interleave x []     = [[x]]
interleave x (y:ys) = (x:y:ys) : (map (y:) (interleave x ys))

-- | Given a list, returns a list of lists comprising all permutations of its
-- elements.
permutations :: [a] -> [[a]]
permutations []        = [[]]
permutations (x:xs)    = concat (map (interleave x) (permutations xs))

-- | Given a list, returns all possible sublists which can be formed by
-- selecting some (or none) of its elements, possibly in a different order.
-- Example: choices [1, 2] = [[], [1], [2], [1, 2], [2, 1]]
choices :: [a] -> [[a]]
choices = concat . map permutations . sublists

-- | Decides whether a valid expression is a solution of the problem using a
-- given list of numbers (or some sublist of it) for a given target.
isSolution :: Expr -> [Int] -> Int -> Bool
isSolution e ns target = elem (values e) (choices ns) && eval e == [target]

-- | Given a list xs, returns all possible pairs (ls, rs) consisting of
-- nonempty sublists which concatenate to xs, in the form of a list of pairs.
split :: [a] -> [([a], [a])]
split []        = []
split [_]       = []
split (x:xs)    = ([x], xs) : [(x:ls, rs) | (ls, rs) <- split xs]

-- | Returns a list containing all possible combinations of two given
-- expressions using one of the four elementary operations.
combine :: Expr -> Expr -> [Expr]
combine l r = [App o l r | o <- ops]

-- | Given a list of integers, returns a list of all possible expressions
-- obtained by combining them in the given order, but using any possible
-- association and any of the elementary operations at each step.
expressions :: [Int] -> [Expr]
expressions []    = []
expressions [n]   = [Val n]
expressions ns    = [e | (ls, rs) <- split ns,
                          l <- expressions ls,
                          r <- expressions rs,
                          e <- combine l r] 

-- | Given a list of integers and a target, returns the list of all solutions
-- (if any) to the countdown problem for these inputs.
solutions :: [Int] -> Int -> [Expr]
solutions ns n = [e | cs <- choices ns, e <- expressions cs, eval e == [n]]

-- | Prints the number of solutions to the countdown problem given a list of
-- integers and a target.
main :: IO ()
main = print(length(solutions [1, 3, 7, 10, 25, 50] 765))
