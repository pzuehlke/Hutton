---------------------------------
--  Binary String Transmitter  --
---------------------------------

import Data.Char


type Bit = Int

-- | Converts a (reverse) list of bits to a positive integer.
-- Example: bitToInt [1, 0, 1, 1] = 13.
bitToInt :: [Bit] -> Int
bitToInt = (foldr (\x y -> x + 2 * y) 0)
{--
bitToInt bits = sum [w * b | (w, b) <- zip weights (reverse bits)]
                where
                  weights = iterate (*2) 1
--}

-- | Convert a positive integer to a (reverse) list of bits. Any number <= 0
-- gets mapped to the empty list. Example: intToBit 13 = [1, 0, 1, 1].
intToBit :: Int -> [Bit]
intToBit n  | n <= 0    = []
            | otherwise = (n `mod` 2) : intToBit (n `div` 2) 

-- | Given a (reverse) list of bits, produces a byte by taking only the first
-- eight bits, if necessary also filling the missing bits with 0.
makeByte :: [Bit] -> [Bit]
makeByte bits = take 8 (bits ++ repeat 0)

-- | Encode a string as a list of bytes.
encode :: String -> [[Bit]]
encode = map (makeByte . intToBit . ord)

decode :: [[Bit]] -> String
decode = map (chr . bitToInt)

-- | Simulates a perfect comunication channel to send/receive lists of bits.
channel :: [[Bit]] -> [[Bit]]
channel = id

-- | Simulates the transmission of a string of characters as a list of bits.
transmit :: String -> String
transmit = decode. channel . encode
