---------------------------------------------------
--  CHAPTER 1 - PROGRAMMING IN HASKELL - HUTTON  --
---------------------------------------------------

isEven :: Integral a => a -> Bool
isEven n   = n `mod` 2 == 0

splitAt :: Int -> [a] -> ([a], [a])
splitAt n xs = (take n xs, drop n xs)

reciprocal :: Fractional a => a -> a
reciprocal n = 1 / n

-- absoluteValue :: Integral a => a -> a
-- absoluteValue x = if x >= 0 then x else -x

absoluteValue :: Integral a => a -> a
absoluteValue n | n >= 0    = n
                | otherwise = -n

sign :: Int -> Int
sign n  | n > 0     = n
        | n < 0     = -n
        | otherwise = 0

{-
not :: Bool -> Bool
not False   = True
not True    = False

(&&) :: Bool -> Bool -> Bool
True && True    = True
True && False   = False
False && True   = False
False && False  = False

(&&) :: Bool -> Bool -> Bool
True && True    = True
_   && _        = False

fst :: (a, b) -> a
fst (x, _) = x

snd :: (a, b) -> b
snd (_, y) = y

test :: [Char] -> Bool
test ['a', _, _]    = True
test _              = False

head :: [a] -> a
head (x:_) = x

tail :: [a] -> a
tail (_:xs) -> xs

const :: a -> b -> a
const x = \_ -> x

odds :: Int -> [Int]
odds n =    map f [0..(n - 1)]
            where f x = 2 * x + 1

odds n = map (\x -> 2 * x + 1) [0..(n - 1)]
-}
