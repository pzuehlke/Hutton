-----------------------------
--  Implementation of Nim  --
-----------------------------

import Data.Char

next :: Int -> Int
next 1 = 2
next 2 = 1

type Board = [Int]

initial :: Board
initial = [5, 4, 3, 2, 1]

finished :: Board -> Bool
finished = all (== 0)

valid :: Board -> Int -> Int -> Bool
valid board row num = 1 <= num && num <= board !! (row - 1)

move :: Board -> Int -> Int -> Board
move board row num = [update r n | (r, n) <- zip [1..] board] where
    update r n = if r == row then n - num else n

printRow :: Int -> Int -> IO ()
printRow row num = do
    putStr (show row)
    putStr ":"
    putStrLn (concat (replicate num " *"))

printBoard :: Board -> IO ()
printBoard [a, b, c, d, e] = do
    printRow 1 a
    printRow 2 b
    printRow 3 c
    printRow 4 d
    printRow 5 e

getDigit :: String -> IO Int
getDigit prompt = do
    putStr prompt
    d <- getChar
    newline
    if isDigit d
        then return (digitToInt d)
    else do
        putStrLn "ERROR: Invalid digit"
        getDigit prompt

newline :: IO ()
newline = putChar '\n'

play :: Board -> Int -> IO ()
play board player = do
    newline
    printBoard board
    if finished board
        then do
            putStr "Player "
            putStr (show (next player))
            putStrLn " wins!"
        else do
            newline
            putStr "Player "
            putStrLn (show player)
            row <- getDigit "Enter a row number: "
            num <- getDigit "Stars to remove: "
            if valid board row num
                then play (move board row num) (next player)
            else do
                newline
                putStrLn "ERROR: Invalid move!"
                play board player

nim :: IO ()
nim = play initial 1
