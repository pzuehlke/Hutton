---------------------------------
--  Binary String Transmitter  --
---------------------------------
import Data.Char

type Bit = Int

-- | Converts a (reverse) list of bits to a positive integer.
-- Example: bitToInt [1, 0, 1, 1] = 13 (not 11).
bitToInt :: [Bit] -> Int
bitToInt = (foldr (\x y -> x + 2 * y) 0)

-- | Converts a positive integer to a (reverse) list of bits. Any number <= 0
-- gets mapped to the empty list. Example: intToBit 13 = [1, 0, 1, 1].
intToBit :: Int -> [Bit]
intToBit n  | n <= 0    = []
            | otherwise = (n `mod` 2) : intToBit (n `div` 2) 

-- | Given a (reverse) list of bits, produces a byte by taking only the first
-- eight bits, if necessary also filling the missing bits with 0.
makeByte :: [Bit] -> [Bit]
makeByte bits = take 8 (bits ++ repeat 0)

-- | Counts the number of 1's in a list of bits, modulo 2.
countOnes :: [Bit] -> Int
countOnes = (`mod` 2) . length . filter (== 1)

-- | Adds a parity bit to the end of a list of bits.
addParityBit :: [Bit] -> [Bit]
addParityBit bits = bits ++ [countOnes bits]

-- | Checks the parity of an extended byte (consisting of nine bits). If the
-- check is successful, remove the parity bit; otherwise return error message.
checkParity :: [Bit] -> [Bit]
checkParity bits    | countOnes bs == last bits = take 8 bits
                    | otherwise                 = error "parity error"
                  where
                    bs = take 8 bits

-- | Encodes a string as a list of bytes, extended by a ninth 'parity bit'.
encode :: String -> [[Bit]]
encode = map (addParityBit . makeByte . intToBit . ord)

-- | Decodes a list of extended bytes, returning the resulting string.
decode :: [[Bit]] -> String
decode = map (chr . bitToInt . checkParity)

-- | Simulates a perfect communication channel to send/receive lists of bits.
channel :: [[Bit]] -> [[Bit]]
channel = id

-- | Simulates the transmission of a string of characters as a list of bits.
transmit :: String -> String
transmit = decode . channel . encode

-- Sample test: 
-- transmit "Test successful!"
