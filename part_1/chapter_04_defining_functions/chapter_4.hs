---------------------------------------------------
--  CHAPTER 1 - PROGRAMMING IN HASKELL - HUTTON  --
---------------------------------------------------

isEven :: Integral a => a -> Bool
isEven n   = n `mod` 2 == 0

splitAt :: Int -> [a] -> ([a], [a])
splitAt n xs = (take n xs, drop n xs)

reciprocal :: Fractional a => a -> a
reciprocal n = 1 / n

absoluteValue :: (Num a, Ord a) => a -> a      -- Ord for comparisons
absoluteValue x = if x >= 0 then x else -x

absoluteValue' :: (Num a, Ord a) => a -> a
absoluteValue' n | n >= 0    = n
                 | otherwise = -n

sign :: (Num a, Ord a) => a -> a
sign n  | n > 0     = 1
        | n == 0    = 0
        | otherwise = -1

not :: Bool -> Bool
not False   = True
not True    = False

(&&) :: Bool -> Bool -> Bool
True  && True   = True
True  && False  = False
False && True   = False
False && False  = False

(&&&) :: Bool -> Bool -> Bool
True &&& True    = True
_    &&& _       = False

(&&&&) :: Bool -> Bool -> Bool
True  &&&& b = b
False &&&& _ = False

fst :: (a, b) -> a
fst (x, _) = x

snd :: (a, b) -> b
snd (_, y) = y

beginsWithA :: [Char] -> Bool   -- same as the book's `test`
beginsWithA ['a', _, _] = True
beginsWithA ['A', _, _] = True
beginsWithA _           = False

beginsWithA' :: [Char] -> Bool   -- same as the book's `test`
beginsWithA' ('a':_) = True
beginsWithA' ('A':_) = True
beginsWithA' _       = False

head :: [a] -> a
head (x:_) = x

tail :: [a] -> [a]
tail (_:xs) = xs

add :: Num a => a -> a -> a
add = \x -> (\y -> x + y)

const :: a -> b -> a
const x _ = x

const' :: a -> (b -> a)
const' x = \_ -> x

odds :: Int -> [Int]
odds n = map f [0..n-1]
         where f x = 2 * x + 1
