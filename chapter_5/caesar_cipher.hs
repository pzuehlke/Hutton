----------------------------------------------------
--  CAESAR CIPHER ENCODER AND DECODER IN HASKELL  --
----------------------------------------------------
-- Suggested by the implementation described in chapter 5 of:
-- Hutton, G. - Programming in Haskell

import Data.Char

count :: Eq a => a -> [a] -> Int
count _ []                  = 0
count x (y:ys)  | x == y    = 1 + count x ys
                | otherwise = count x ys

lowerToInt :: Char -> Int
lowerToInt c = ord c - ord 'a'

upperToInt :: Char -> Int
upperToInt c = ord c - ord 'A'

intToLower :: Int -> Char
intToLower n = chr (ord 'a' + n)

intToUpper :: Int -> Char
intToUpper n = chr (ord 'A' + n)

lower :: Char -> Bool
lower c = (ord 'a' <= ord c && ord c <= ord 'z')

upper :: Char -> Bool
upper c = (ord 'A' <= ord c && ord c <= ord 'Z')

numberOfLowers :: String -> Int
numberOfLowers []                   = 0
numberOfLowers (c:cs)   | lower c   = 1 + numberOfLowers cs
                        | otherwise = numberOfLowers cs

numberOfUppers :: String -> Int
numberOfUppers []                   = 0
numberOfUppers (c:cs)   | upper c   = 1 + numberOfUppers cs
                        | otherwise = numberOfUppers cs

shift :: Int -> Char -> Char
shift n c   | lower c       = intToLower ((lowerToInt c + n) `mod` 26)
            | upper c       = intToUpper ((upperToInt c + n) `mod` 26)
            | otherwise     = c

encode :: Int -> String -> String
encode n str = [shift n c | c <- str]

decode :: Int -> String -> String
decode n str = encode (-n) str

frequencies :: [Float]
frequencies = [8.1, 1.5, 2.8, 4.2, 12.7, 2.2, 2.0, 6.1, 7.0,
               0.2, 0.8, 4.0, 2.4, 6.7, 7.5, 1.9, 0.1, 6.0,
               6.3, 9.0, 2.8, 1.0, 2.4, 0.2, 2.0, 0.1]

percent :: Int -> Int -> Float
percent m n = (fromIntegral m / fromIntegral n) * 100

toggleCase :: Char -> Char
toggleCase c    | lower c   = intToUpper (lowerToInt c)
                | upper c   = intToLower (upperToInt c)
                | otherwise = c

freqs :: String -> [Float]
freqs str = [percent (count c str + count (toggleCase c) str) n
             | c <- ['a'..'z']]
            where
              n      = numberOfLowers str + numberOfUppers str

chiSqr :: [Float] -> [Float] -> Float
chiSqr os es = sum [((o - e)^2) / e | (o, e) <- zip os es]

rotate :: Int -> [a] -> [a]
rotate n xs = drop n xs ++ take n xs

positions :: Ord a => a -> [a] -> [Int]
positions x ys = [k | (k, y) <- zip [0..] ys, y == x]

crack :: String -> String
crack str = decode factor str
  where
    os          = freqs str
    es          = frequencies 
    chiTable    = [chiSqr (rotate n os) es | n <- [0..25]]
    factor      = head (positions (minimum chiTable) chiTable)

