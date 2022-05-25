------------------------------------------------------
--  Exercise 10.4 - Programming in Haskell - Hutton  --
------------------------------------------------------

import Data.Char
-- Required for the use of 'isDigit' and 'digitToInt'.

newLine :: IO ()
newLine = putChar '\n'

adder :: IO ()
adder = do putStrLn "How many digits do you want do add? "
           n <- getChar
           newLine
           if isDigit n && (digitToInt n >= 0) then
              adderIter 0 (digitToInt n)
           else do error "Input is not a digit."
                   adder
           return ()

adderIter :: Int -> Int -> IO ()
adderIter total n | n == 0      = do putStr "The total is: "
                                     putStr (show total)
                                     newLine
                                     return ()
                  | otherwise   = do putStrLn "Type in a summand: "
                                     m <- getChar
                                     newLine
                                     if isDigit m then
                                        do adderIter (total + (digitToInt m)) (n - 1)
                                     else do error "Input is not a digit."
                                             adderIter total n
