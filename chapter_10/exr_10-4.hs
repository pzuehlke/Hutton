------------------------------------------------------
--  Exercise 10.4 - Programming in Haskell - Hutton  --
------------------------------------------------------

import Data.Char (isDigit, digitToInt)

adder :: IO ()
adder = do
    putStrLn "How many digits do you want do add? "
    numbersToAdd <- getChar
    putChar '\n'
    if isDigit numbersToAdd && (digitToInt numbersToAdd >= 0)
        then adderIter 0 (digitToInt numbersToAdd)
        else do
            putStrLn "ERROR: Input is not a digit. Try again."
            adder
    return ()

adderIter :: Int -> Int -> IO ()
adderIter total numbersToAdd
    | numbersToAdd == 0 = do
        putStr "The total is "
        putStrLn (show total)
        return ()
    | otherwise = do
        putStrLn "Type in a summand: "
        n <- getChar
        putChar '\n'
        if isDigit n
            then adderIter (total + (digitToInt n)) (numbersToAdd - 1)
            else do
                putStrLn "ERROR: Input is not a digit."
                adderIter total numbersToAdd
