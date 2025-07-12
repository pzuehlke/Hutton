----------------------------------------------------
--  Chapter 16 - Programming in Haskell - Hutton  --
----------------------------------------------------

data Nat = Zero | Succ Nat

double :: Int -> Int
double x = x + x

isZero :: Int -> Bool
isZero 0 = True
isZero n = False

isZero' :: Int -> Bool
isZero' 0           = True
isZero' n | n /= 0  = False

myReverse :: [a] -> [a]
myReverse []        = []
myReverse (x:xs)    = myReverse xs ++ [x]

myNot :: Bool -> Bool
myNot True  = False
myNot False = True

add :: Nat -> Nat -> Nat
add Zero        n = n
add (Succ m)    n = Succ(add m n)

myReplicate :: Int -> a -> [a]
myReplicate 0 _ = []
myReplicate n x = x : (myReplicate (n - 1) x)

fmap :: (a -> b) -> [a] -> [b]
fmap g []       = []
fmap g (x:xs)   = g x : fmap g xs

-- Compiler correctness
data Expr = Val Int | Add Expr Expr

eval :: Expr -> Int
eval (Val n)    = n
eval (Add x y)  = eval x + eval y

type Stack = [Int]
data Op = PUSH Int | ADD deriving Show
type Code = [Op]

exec :: Code -> Stack -> Stack
exec []             s           = s
exec (PUSH n : c)   s           = exec c (n : s)
exec (ADD : c)      (m : n : s) = exec c (n + m : s)

comp :: Expr -> Code
comp (Val n)    = [PUSH n]
comp (Add x y)  = comp x ++ comp y ++ [ADD]

e :: Expr
-- e = (2 + 3) + 4
e = Add (Add (Val 2) (Val 3)) (Val 4)
