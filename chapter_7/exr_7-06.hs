------------------------------------------------------
--  Exercise 7.6 - Programming in Haskell - Hutton  --
------------------------------------------------------

type Bit = Int

unfold :: (a -> Bool) -> (a -> b) -> (a -> a) -> a -> [b]
unfold p h t x  | p x       = []
                | otherwise = h x : unfold p h t (t x)

intToBin :: Int -> [Bit]
intToBin = unfold (== 0) (`mod` 2) (`div` 2)
-- Include 'reverse . ' to the left of unfold to obtain the list of bits in the
-- usual order. Example:
-- intToBin 9   = 1 : (unfold (==0) (`mod 2`) (`div 2`) 4)
--              = 1 : 0 : (unfold (==0) (`mod`2) (`div` 2) 2)
--              = 1 : 0 : 0 : (unfold (==0) (`mod`2) (`div` 2) 1)
--              = 1 : 0 : 0 : 1 : (unfold (==0) (`mod`2) (`div` 2) 0)
--              = 1 : 0 : 0 : 1 : []
--              = [1, 0, 0, 1]

chop8 :: [Bit] -> [[Bit]]
chop8 = unfold (== []) (take 8) (drop 8)
-- Test: chop8 [1,0,0,0,0,1,1,0, 0,1,0,0,0,1,1,0, 1,1,0,0,0,1,1,0]

myMap :: Eq a => (a -> b) -> [a] -> [b]
myMap f = unfold (== []) (f . head) tail
-- Test: myMap (*2) [1, 2, 3, 4, 5]

myIterate :: (a -> a) -> a -> [a]
myIterate f = unfold (\x -> False) id f
-- Test: take 10 (myIterate (*2) 1)
