---------------------------------------------------
--  CHAPTER 7 - PROGRAMMING IN HASKELL - HUTTON  --
---------------------------------------------------

-- Section 7.1

add :: Int -> Int -> Int
add = \x -> (\y -> x + y)

twice :: (a -> a) -> a -> a
twice f x = f (f x)

-- Section 7.2

myMap :: (a -> b) -> [a] -> [b]
myMap f []      = []
myMap f (x:xs)  = (f x) : (myMap f xs)

myMap' :: (a -> b) -> [a] -> [b]
myMap' f xs = [f x | x <- xs]

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter p xs = [x | x <- xs, p x]

myFilter' :: (a -> Bool) -> [a] -> [a]
myFilter' p []                 = []
myFilter' p (x:xs) | p x       = x : (myFilter' p xs)
                   | otherwise = myFilter' p xs

sumSqrEven :: [Int] -> Int
sumSqrEven ns = sum (map (^2) (filter even ns))

myAll :: (a -> Bool) -> [a] -> Bool
myAll p []      = True
myAll p (x:xs)  = (p x) && (myAll p xs)

myAny :: (a -> Bool) -> [a] -> Bool
myAny p []      = False
myAny p (x:xs)  = (p x) || (myAny p xs)

myTakeWhile :: (a -> Bool) -> [a] -> [a]
myTakeWhile p []                    = []
myTakeWhile p (x:xs)    | p x       = x : myTakeWhile p xs
                        | otherwise = []

myDropWhile :: (a -> Bool) -> [a] -> [a]
myDropWhile p []                    = []
myDropWhile p (x:xs)    | p x       = myDropWhile p xs
                        | otherwise = (x:xs)

-- Section 7.3

mySum :: Num a => [a] -> a
mySum []        = 0
mySum (x:xs)    = x + mySum xs

myProduct :: Num a => [a] -> a
myProduct []        = 1
myProduct (x:xs)    = x * myProduct xs

myOr :: [Bool] -> Bool
myOr []     = False
myOr (x:xs) = x || myOr xs

myAnd :: [Bool] -> Bool
myAnd (x:xs)    = x && myAnd xs
myAnd []        = True

mySum' :: Num a => [a] -> a
mySum' = foldr (+) 0

myProduct' :: Num a => [a] -> a
myProduct' = foldr (*) 1

myOr' :: [Bool] -> Bool
myOr' = foldr (||) False

myAnd' :: [Bool] -> Bool
myAnd' = foldr (&&) True

myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr f v []      = v
myFoldr f v (x:xs)  = f x (myFoldr f v xs)

myLength :: [a] -> Int
myLength = foldr (\x n -> n + 1) 0

myReverse :: [a] -> [a]
myReverse = foldr (\x xs -> xs ++ [x]) []

-- Section 7.4

mySum'' :: Num a => [a] -> a
mySum'' (x:xs)  = x + mySum'' xs

-- Section 7.5

comp :: (b -> c) -> (a -> b) -> (a -> c)
f `comp` g = \x -> f (g x)

myCompose :: [a -> a] -> (a -> a)
myCompose []        = id
myCompose (f:fs)    = f . myCompose fs
