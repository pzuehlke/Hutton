---------------------------------------------------
--  Chapter 8 - Programming in Haskell - Hutton  --
---------------------------------------------------

-- Section 8.1
type Pos = (Int, Int)
type Trans = Pos -> Pos
type Pair a = (a, a)
type Assoc k v = [(k, v)]

findFirst :: Eq k => k -> Assoc k v -> v
findFirst k dict = head [v | (k', v) <- dict, k' == k]

-- Section 8.2
-- data Bool = False | True
data Move = South | North | West | East deriving Show

move :: Move -> Pos -> Pos
move South (x, y) = (x, y - 1)
move North (x, y) = (x, y + 1)
move West (x, y) = (x - 1, y)
move East (x, y) = (x + 1, y)

moves :: [Move] -> Pos -> Pos
moves [] p      = p
moves (m:ms) p  = move m (moves ms p)    

reverse :: Move -> Move
reverse South = North
reverse North = South
reverse West = East
reverse East = West

data Shape = Circle Float | Rectangle Float Float

square :: Float -> Shape
square x = Rectangle x x

area :: Shape -> Float
area (Circle r)       = pi * r^2
area (Rectangle x y)  = x * y

-- data Maybe a = Nothing | Just a

safeDiv :: Int -> Int -> Maybe Int
safeDiv n 0     = Nothing
safeDiv n m     = Just (n `div` m)

safeHead :: [a] -> Maybe a
safeHead []     = Nothing
safeHead xs     = Just (head xs)


-- Section 8.3
-- newtype Nat = N Int

-- Section 8.4
data Nat = Zero | Succ Nat

natToInt :: Nat -> Int
natToInt Zero       = 0
natToInt (Succ n)     = natToInt n + 1

intToNat :: Int -> Nat
intToNat 0 = Zero
intToNat n = Succ (intToNat (n - 1))

add :: Nat -> Nat -> Nat
-- add m n     = intToNat(natToInt m + natToInt n)
add Zero n          = n
add (Succ m) n      = Succ (add m n)

data List a = Nil | Cons a (List a)

len :: List a -> Int
len Nil             = 0
len (Cons _ xs)     = 1 + len xs

data Tree a = Leaf a | Node (Tree a) a (Tree a)

occurs :: Eq a => a -> Tree a -> Bool
occurs x (Leaf y)       = x == y
occurs x (Node l y r)   = x == y || occurs x l || occurs x r

flatten :: Tree a -> [a]
flatten (Leaf x)    = [x]
flaten (Node l x r) = flatten l ++ [x] ++ flatten r

occurs' :: Ord a => a -> Tree a -> Bool
occurs' x (Leaf y)                  = x == y
occurs' x (Node l y r)  | x == y    = True
                        | x < y     = occurs' x l
                        | otherwise = occurs' x r

{--
data Tree a = Leaf a | Node (Tree a) (Tree a)
data Tree a = Leaf | Node (Tree a) a (Tree a)
data Tree a b = Leaf a | Node (Tree b) a (Tree b)
data Tree a = Node a [Tree a]
--}

-- Section 8.5
{--
class Eq a where
  (==), (/=) :: a -> a -> Bool
    
  x /= y = not (x == y)

instance Eq Bool where
  False == False    = True
  True == True      = True 
  _ == _            = False

class Eq a => Ord a where
  (<), (<=), (>), (>=)  :: a -> a -> Bool
  min, max              :: a -> a-> a

  min x y | x <= y      = x
          | otherwise   = y

  max x y | x <= y      = y
          | otherwise   = x
  
  (>) x y   = y < x

  (<=) x y  = (x == y) || (x < y)

  (>=) x y  = (x == y) || (x > y)

instance Ord Bool where
  False < True  = True
  _     < _     = False

data Bool = False | True
            deriving (Eq, Ord, Show, Read)
--}
