---------------------------------------------------
--  CHAPTER 5 - PROGRAMMING IN HASKELL - HUTTON  --
---------------------------------------------------

concatenate :: [[a]] -> [a]
concatenate xss = [x | xs <- xss, x <- xs]

firsts :: [(a, b)] -> [a]
firsts pairs = [x | (x, _) <- pairs]

listLength :: [a] -> Int
listLength xs = sum [1 | x <- xs]

factors :: Int -> [Int]
factors n = [d | d <- [1..n], n `mod` d == 0]

isPrime :: Int -> Bool
isPrime n = (factors n) == [1, n]

getPrimes :: Int -> [Int]
getPrimes n = [p | p <- [2..n], isPrime p]

find :: Eq a => a -> [(a, b)] -> [b]
find k tuples = [value | (key, value) <- tuples, key == k]

myZip :: [a] -> [b] -> [(a, b)]
myZip [] _          = []
myZip _  []         = []
myZip (x:xs) (y:ys) = (x, y) : myZip xs ys

pairs :: [a] -> [(a, a)]
pairs xs = zip xs (tail xs)

isSorted :: Ord a => [a] -> Bool
isSorted xs = and [x <= y | (x, y) <- pairs xs]

positions :: Eq a => a -> [a] -> [Int]
positions x xs = [i | (y, i) <- myZip xs [0..], y == x]

lowers :: String -> Int
lowers xs = length [x | x <- xs, 'a' <= x && x <= 'z']

count :: Char -> String -> Int
count c string = length [char | char <- string, char == c]
