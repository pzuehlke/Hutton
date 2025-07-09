----------------------------------------------------
--  Chapter 15 - Programming in Haskell - Hutton  --
----------------------------------------------------

import Data.Foldable

inc :: Int -> Int
inc n = n + 1

mult :: (Int, Int) -> Int
mult (x, y) = x * y

mult' :: Int -> Int -> Int
mult' x = \y -> x * y

inf :: Int
inf = 1 + inf

ones :: [Int]
ones = 1 : ones

replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' n x = x : replicate' (n - 1) x

primes :: [Int]
primes = sieve [2..]

sieve :: [Int] -> [Int]
sieve (p:xs) = p : sieve [n | n <- xs, n `mod` p /= 0]

sumwith :: Int -> [Int] -> Int
sumwith v []        = v
sumwith v (x:xs)    = sumwith (v + x) xs

sumwith' :: Int -> [Int] -> Int
sumwith' = foldl' (+)
