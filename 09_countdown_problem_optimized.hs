-------------------------------------------------------------
--  The Countdown Problem Solver - More Efficient Solution --
-------------------------------------------------------------
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

-- | Elements of this type consist of an expression paired with its value
-- (as long as the expression evaluates successfully).
type Result = (Expr, Int)

-- List containing all members of Op:
ops :: [Op]
ops = [Add, Sub, Mul, Div]

-- | Checks if the application of an elementary operation to two integers is
-- valid according to the rules of the game.
valid :: Op -> Int -> Int -> Bool
valid Add m n   = m <= n
valid Sub m n   = m > n
valid Mul m n   = m /= 1 && n /= 1 && m <= n
valid Div m n   = n /= 1 && m `mod` n == 0

-- | Applies an elementary operation to two integers.
apply :: Op -> Int -> Int -> Int
apply Add m n   = m + n
apply Sub m n   = m - n
apply Mul m n   = m * n
apply Div m n   = m `div` n

-- Some sample expressions:
-- (1 + 50) * (25 - 10)
e :: Expr
e = App Mul (App Add (Val 1) (Val 50)) (App Sub (Val 25) (Val 10))

-- 1 + (2 * 3)
e1 :: Expr
e1 = App Add (Val 1) (App Mul (Val 2) (Val 3))

-- (4 / 2) - 1
e2 :: Expr
e2 = App Sub (App Div (Val 4) (Val 2)) (Val 1)

-- 2 - 3
e3 :: Expr
e3 = App Sub (Val 2) (Val 3)

-- (2 / 3) * 3
e4 :: Expr
e4 = App Mul (App Div (Val 2) (Val 3)) (Val 3)

-- (2 * 3) / 3
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
subs :: [a] -> [[a]]
subs []         = [[]]
subs (x:xs)     = (subs xs) ++ (map (x:) (subs xs))

-- | Given x and and a list xs, returns a list of lists comprising all possible
-- ways to obtain a new list by inserting x into xs.
interleave :: a -> [a] -> [[a]]
interleave x []     = [[x]]
interleave x (y:ys) = (x:y:ys) : (map (y:) (interleave x ys))

-- | Given a list, returns a list of lists comprising all permutations of its
-- elements.
perms :: [a] -> [[a]]
perms []        = [[]]
perms (x:xs)    = concat (map (interleave x) (perms xs))

-- | Given a list, returns all possible sublists which can be formed by
-- selecting some (or none) of its elements, possibly in a different order.
-- Example: choices [1, 2] = [[], [1], [2], [1, 2], [2, 1]]
choices :: [a] -> [[a]]
choices = concat . map perms . subs

-- | Decides whether a valid expression is a solution of the problem using a
-- given list of numbers (or some sublist of it) for a given target.
isSolution :: Expr -> [Int] -> Int -> Bool
isSolution e ns target = elem (values e) (choices ns) && eval e == [target]

-- | Given a list xs, returns all possible pairs (ls, rs) consisting of
-- sublists which concatenate to xs, in the form of a list of pairs.
split :: [a] -> [([a], [a])]
split []        = []
split [_]       = []
split (x:xs)    = ([x], xs) : [(x:ls, rs) | (ls, rs) <- split xs]

-- | Returns a list containing all (at most four) possible valid results of
-- combining two given expressions using one of the elementary operations.
combine' :: Result -> Result -> [Result]
combine' (l, m) (r, n) = [(App o l r, apply o m n) | o <- ops, valid o m n]

-- | Given a list of integers, returns the list of all possible results (pairs
-- of valid expressions and their values) that can be formed from them.
results :: [Int] -> [Result]
results []  = []
results [n] = [(Val n, n) | n > 0]
results ns  = [res | (ls, rs)   <- split ns,
                     lx         <- results ls,
                     ry         <- results rs,
                     res        <- combine' lx ry]

-- | Given a list of integers and a target, returns the list of all solutions
-- (if any) to the countdown problem for these inputs.
solutions' :: [Int] -> Int -> [Expr]
solutions' ns n = [e | ms <- choices ns, (e, m) <- results ms, m == n]

-- | Prints the number of solutions to the countdown problem given a list of
-- integers and a target.
main :: IO ()
main = print(length(solutions' [1, 3, 7, 10, 25, 50] 765))
