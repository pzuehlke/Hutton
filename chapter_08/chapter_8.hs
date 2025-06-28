---------------------------------------------------
--  Chapter 8 - Programming in Haskell - Hutton  --
---------------------------------------------------

-- Section 8.1
type String = [Char]
type Pos = (Int, Int)
type Trans = Pos -> Pos
-- type Tree = (Int, [Tree])    wrong: type declarations cannot be recursive

type Pair a = (a, a)
type Assoc k v = [(k, v)]

find :: Eq k => k -> Assoc k v -> v
find k t = head [v | (k', v) <- t, k' == k]


-- Section 8.2
-- data Bool = False | True
data Move = North | South | East | West deriving Show

move :: Move -> Pos -> Pos
move North (x, y) = (x, y + 1)
move South (x, y) = (x, y - 1)
move East  (x, y) = (x + 1, y)
move West  (x, y) = (x - 1, y)

moves :: [Move] -> Pos -> Pos
moves ms p = foldl (\position m -> move m position) p ms
-- moves []     p = p
-- moves (m:ms) p = moves ms (move m p)

rev :: Move -> Move
rev North = South
rev South = North
rev East  = West
rev West  = East

data Shape = Circle Float | Rect Float Float
square :: Float -> Shape
square x = Rect x x

area :: Shape -> Float
area (Circle r) = pi * r^2
area (Rect x y) = x * y

-- data Maybe a = Nothing | Just a

safediv :: Int -> Int -> Maybe Int
safediv _ 0 = Nothing
safediv n d = Just (n `div` d)

safehead :: [a] -> Maybe a
safehead [] = Nothing
safehead xs = Just (head xs)

-- Section 8.3
newtype Nat' = N Int      -- will be redefined below

-- Section 8.4
data Nat = Zero | Succ Nat

natToInt :: Nat -> Int
natToInt Zero       = 0
natToInt (Succ n)   = 1 + natToInt n

intToNat :: Int -> Nat
intToNat n
    | n <= 0    = Zero
    | otherwise = Succ (intToNat (n - 1))

add :: Nat -> Nat -> Nat
add m n = intToNat (natToInt m + natToInt n)

add' :: Nat -> Nat -> Nat
add' Zero     n = n
add' (Succ m) n = Succ (add' m n)


-- Section 8.5
{--
class Eq a where
    (==), (/=) :: a -> a -> Bool

x /= y = not (x == y)

instance Eq Bool where
    False == False = True
    True  == True  = True
    _     == _     = False

class Eq a => Ord a where
    (<), (<=), (>), (>=) :: a -> a -> Bool
    min, max             :: a -> a -> a

min x y
    | x <= y    = x
    | otherwise = y

max x y
    | x <= y    = y
    | otherwise = x

instance Ord Bool where
    False < True    = True
    _     < _       = False     

b <= c = (b < c) || (b == c)
b > c  = c < b
b >= c = c <= b

data Bool = False | True
    deriving (Eq, Ord, Show, Read)
--}
