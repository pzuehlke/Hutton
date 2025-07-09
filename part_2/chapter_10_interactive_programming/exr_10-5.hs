------------------------------------------------------
--  Exercise 10.5 - Programming in Haskell - Hutton  --
------------------------------------------------------

import Data.Char (isDigit, digitToInt)

getCharLn :: IO Char
getCharLn = do
    c <- getChar
    putChar '\n'
    return c

adder :: IO ()
adder = do
    putStr "How many digits do you want do add? "
    numbersToAdd <- getCharLn
    if isDigit numbersToAdd && (digitToInt numbersToAdd >= 0)
        then do
            putStrLn "Type in the digits, one after the other:"
            summands <- sequence [getCharLn |
                                  _ <- [1..(digitToInt numbersToAdd)]]
            putStr "The total is "
            putStrLn (show (sum (map digitToInt summands)))
        else do
            putStrLn "Input is not a digit."
            adder
    return ()
