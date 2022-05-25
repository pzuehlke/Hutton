------------------------------------------------------
--  Exercise 10.5 - Programming in Haskell - Hutton  --
------------------------------------------------------

import Data.Char

adder :: IO ()
adder = do putStrLn "How many digits do you want do add? "
           n <- getChar
           newLine
           if isDigit n && (digitToInt n >= 0) then
              do putStrLn "Type in the digits, one after the other: "
                 adderIter (digitToInt n)
           else do error "Input is not a digit."
                   adder
           return ()

newLine :: IO ()
newLine = putChar '\n'

adderIter :: Int -> IO ()
adderIter n = do summands <- sequence [getChar | _ <- [1..n]]
                 newLine
                 putStr "The total is: "
                 putStrLn (show (sum (map digitToInt summands)))
                 return ()
