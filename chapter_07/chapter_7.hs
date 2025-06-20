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
myMap _ []      = []
myMap f (x:xs)  = f x : myMap f xs

myMap' :: (a -> b) -> [a] -> [b]
myMap' f xs = [f x | x <- xs]

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter p xs = [x | x <- xs, p x]

myFilter' :: (a -> Bool) -> [a] -> [a]
myFilter' p []                  = []
myFilter' p (x:xs)  | p x       = x : myFilter' p xs
                    | otherwise = myFilter p xs

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
myAnd []        = True
myAnd (x:xs)    = x && myAnd xs

mySum' :: Num a => [a] -> a
mySum' = foldr (+) 0

myProduct' :: Num a => [a] -> a
myProduct' = foldr (*) 1

myOr' :: [Bool] -> Bool
myOr' = foldr (||) False

myAnd' :: [Bool] -> Bool
myAnd' = foldr (&&) True

myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr op v []      = v
myFoldr op v (x:xs)  = x `op` (myFoldr op v xs)

myLength :: [a] -> Int
myLength = foldr (\_ n -> 1 + n) 0

myReverse :: [a] -> [a]
myReverse = foldr (\x xs -> xs ++ [x]) []

-- Section 7.4

mySum'' :: Num a => [a] -> a
mySum'' = foldl (+) 0

myProduct'' :: Num a => [a] -> a
myProduct'' = foldl (*) 1

myOr'' :: [Bool] -> Bool
myOr'' = foldl (||) False

myAnd'' :: [Bool] -> Bool
myAnd'' = foldl (&&) True

myFoldl :: (b -> a -> b) -> b -> [a] -> b
myFoldl op v []     = v
myFoldl op v (x:xs) = myFoldl op (v `op` x) xs

myLength :: [a] -> Int
myLength = foldr (\_ n -> 1 + n) 0

myReverse :: [a] -> [a]
myReverse = foldr (\x xs -> xs ++ [x]) []

-- Section 7.5

comp :: (b -> c) -> (a -> b) -> (a -> c)
f `comp` g = \x -> f (g x)

compose :: [a -> a] -> (a -> a)
compose = foldr (.) id

compose' :: [a -> a] -> (a -> a)
compose' []        = id
compose' (f:fs)    = f . compose' fs
