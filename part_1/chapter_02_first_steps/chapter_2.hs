---------------------------------------------------
--  CHAPTER 2 - PROGRAMMING IN HASKELL - HUTTON  --
---------------------------------------------------

factorial :: Int -> Int
factorial 0 = 1
factorial n = product [1..n]

average :: [Float] -> Float
average [] = 0
average xs = sum xs / fromIntegral (length xs)
