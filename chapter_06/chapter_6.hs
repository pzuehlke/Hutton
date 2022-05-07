---------------------------------------------------
--  CHAPTER 6 - PROGRAMMING IN HASKELL - HUTTON  --
---------------------------------------------------

factorial :: Int -> Int
factorial n = product [1..n]

factorial' :: Int -> Int
factorial' 0 = 1
factorial' n = n * factorial' (n - 1)

mul :: Int -> Int -> Int
mul m 0 = 0
mul m n = m + mul m (n - 1)

myProduct :: Num a => [a] -> a
myProduct [] = 1
myProduct (n:ns) = n * myProduct ns

myLength :: [a] -> Int
myLength [] = 0
myLength (_:xs) = 1 + myLength xs

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

myInsert :: Ord a => a -> [a] -> [a]
myInsert x []                   = [x] 
myInsert x (y:ys)   | x <= y    = x : y : ys
                    | otherwise = y : (myInsert x ys)

insertionSort :: Ord a => [a] -> [a]
insertionSort []        = []
insertionSort (x : xs)  = myInsert x xs

myEven :: Int -> Bool
myEven 0 = True
myEven n    | n > 0     = myOdd (n - 1)
            | n < 0     = myOdd (n + 1)

myOdd :: Int -> Bool
myOdd 0 = False
myOdd n     | n > 0     = myEven (n - 1)
            | n < 0     = myEven (n + 1)

myZip :: [a] -> [b] -> [(a, b)]
myZip [] _          = []
myZip _ []          = []
myZip (x:xs) (y:ys) = (x, y) : myZip xs ys

myDrop :: Int -> [a] -> [a]
myDrop _ []     = []
myDrop 0 xs     = xs
myDrop n (_:xs) = myDrop (n - 1) xs

quickSort :: Ord a => [a] -> [a]
quickSort []        = []
quickSort (x:xs)    = quickSort smaller ++ [x] ++ quickSort larger
                      where  
                        smaller = [y | y <- xs, y < x]
                        larger = [y | y <- xs, x >= y]

evens :: [a] -> [a]
evens []        = []
evens (x:xs)    = x : odds xs

odds :: [a] -> [a]
odds []        = []
odds (x:xs)    = evens xs

myInit :: [a] -> [a]
myInit []       = []
myInit [_]      = []
myInit (x:xs)   = x : myInit xs
