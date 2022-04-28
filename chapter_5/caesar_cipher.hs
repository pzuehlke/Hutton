----------------------------------------------------
--  CAESAR CIPHER ENCODER AND DECODER IN HASKELL  --
----------------------------------------------------
-- Suggested by the implementation described in chapter 5 of:
-- Hutton, G. - Programming in Haskell

import Data.Char

-- | Count the number of occurrences of an item in a list.
count :: Eq a => a -> [a] -> Int
count _ []                  = 0
count x (y:ys)  | x == y    = 1 + count x ys
                | otherwise = count x ys

-- | Convert a lowercase letter to an integer between 0 and 25.
lowerToInt :: Char -> Int
lowerToInt c = ord c - ord 'a'

-- | Convert an uppercase letter to an integer between 0 and 25.
upperToInt :: Char -> Int
upperToInt c = ord c - ord 'A'

-- | Convert an integer between 0 and 25 to a lowercase letter.
intToLower :: Int -> Char
intToLower n = chr (ord 'a' + n)

-- | Convert an integer between 0 and 25 to an uppercase letter.
intToUpper :: Int -> Char
intToUpper n = chr (ord 'A' + n)

-- | Check if a character is a lowercase letter.
lower :: Char -> Bool
lower c = (ord 'a' <= ord c && ord c <= ord 'z')

-- | Check if a character is an uppercase letter.
upper :: Char -> Bool
upper c = (ord 'A' <= ord c && ord c <= ord 'Z')

-- | Convert an uppercase letter to the corresponding lowercase letter. For
-- other characters, return the original character.
convToLower :: Char -> Char
convToLower c   | upper c       = intToLower (upperToInt c)
                | otherwise     = c

-- | Count the number of lowercase letters in a string.
numberOfLowers :: String -> Int
numberOfLowers []                   = 0
numberOfLowers (c:cs)   | lower c   = 1 + numberOfLowers cs
                        | otherwise = numberOfLowers cs

-- | Shift a lowercase or an uppercase character by n (mod 26) with respect to
-- its integer representation; returns a character of the same type.
shift :: Int -> Char -> Char
shift n c   | lower c       = intToLower ((lowerToInt c + n) `mod` 26)
            | upper c       = intToUpper ((upperToInt c + n) `mod` 26)
            | otherwise     = c

-- | Encode a string by shifting the integer representation of every lowercase
-- or uppercase character in it by a fixed integer.
encode :: Int -> String -> String
encode n str = [shift n c | c <- str]

-- | Given a string encoded through a shift by n, decode it.
decode :: Int -> String -> String
decode n str = encode (-n) str

-- | Table containing the approximate relative frequencies of all letters of
-- the alphabet (in order) in the English language.
frequencies :: [Float]
frequencies = [8.1, 1.5, 2.8, 4.2, 12.7, 2.2, 2.0, 6.1, 7.0,
               0.2, 0.8, 4.0, 2.4, 6.7, 7.5, 1.9, 0.1, 6.0,
               6.3, 9.0, 2.8, 1.0, 2.4, 0.2, 2.0, 0.1]

-- | Express a proper fraction m / n (with m < n) as a percentage.
percent :: Int -> Int -> Float
percent m n = (fromIntegral m / fromIntegral n) * 100

-- | Compute the frequency table of all 26 letters of the alphabet in a given
-- string. Both lower- and uppercase letters are taken into account.
freqs :: String -> [Float]
freqs str = [percent (count c lowerStr) n | c <- ['a'..'z']]
            where
              lowerStr  = map convToLower str
              n         = numberOfLowers lowerStr

-- | Compute chi^2 for two given lists (of the same length) of observed and
-- expected values, respectively.
chiSqr :: [Float] -> [Float] -> Float
chiSqr os es = sum [((o - e)^2) / e | (o, e) <- zip os es]

-- | Cyclically shift every element of a list by n positions.
rotate :: Int -> [a] -> [a]
rotate n xs = drop n xs ++ take n xs

-- | Given an object and a list, return a list containing all positions where
-- this object occurs in the list (if any).
positions :: Ord a => a -> [a] -> [Int]
positions x ys = [k | (k, y) <- zip [0..] ys, y == x]

-- | Crack the Caesar cipher, i.e., given a string encoded using this method,
-- return the decoded string.
crack :: String -> String
crack str = decode factor str
  where
    os          = freqs str
    es          = frequencies 
    chiTable    = [chiSqr (rotate n os) es | n <- [0..25]]
    factor      = head (positions (minimum chiTable) chiTable)


-- An additional related function, not really necessary for the task:

{--
-- | Toggle between upper- and lowercase characters.
toggleCase :: Char -> Char
toggleCase c    | lower c   = intToUpper (lowerToInt c)
                | upper c   = intToLower (upperToInt c)
                | otherwise = c
--}
