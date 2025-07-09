---------------------------------------------------
--  CHAPTER 5 - PROGRAMMING IN HASKELL - HUTTON  --
---------------------------------------------------

concat :: [[a]] -> [a]
concat xss = [x | xs <- xss, x <- xs]

firsts :: [(a, b)] -> [a]
firsts ps = [x | (x, _) <- ps]

length' :: [a] -> Int
length' xs = sum [1 | _ <- xs]

factors :: Int -> [Int]
factors n = [k | k <- [1..n], n `mod` k == 0]

prime :: Int -> Bool
prime n = factors n == [1, n]

primes :: Int -> [Int]
primes n = [p | p <- [2..n], prime p]

find :: Eq a => a -> [(a, b)] -> [b]
find k t = [v | (k', v) <- t, k' == k]

myZip :: [a] -> [b] -> [(a, b)]
myZip [] _          = []
myZip _  []         = []
myZip (x:xs) (y:ys) = (x, y) : myZip xs ys

pairs :: [a] -> [(a, a)]
pairs xs = zip xs (tail xs)

sorted :: Ord a => [a] -> Bool
sorted xs = and [x <= y | (x, y) <- pairs xs]

positions :: Eq a => a -> [a] -> [Int]
positions x xs = [i | (y, i) <- zip xs [0..], y == x]

lowers :: String -> Int
lowers cs = length [c | c <- cs, 'a' <= c && c <= 'z']

count :: Char -> String -> Int
count c cs = length [c' | c' <- cs, c' == c]
