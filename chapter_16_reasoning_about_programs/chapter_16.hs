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
