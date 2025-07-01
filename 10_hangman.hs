------------------------------------------
--  Implementation of the game Hangman  --
------------------------------------------

import System.IO

hangman :: IO ()
hangman = do
    putStrLn "Think of a word:"
    word <- secretGetLine
    putStrLn "Try to guess it:"
    play word

secretGetLine :: IO String
secretGetLine = do
    char <- getCh
    if char == '\n'
        then do
            putChar char
            return []
        else do
            putChar '-'
            rest <- secretGetLine
            return (char:rest)

getCh :: IO Char
getCh = do
    hSetEcho stdin False
    char <- getChar
    hSetEcho stdin True
    return char

play :: String -> IO ()
play word = do
    putStr "? "
    guess <- getLine
    if guess == word
        then putStrLn "You got it!"
    else do
        putStrLn (match word guess)
        play word

match :: String -> String -> String
match word guess = [if elem letter guess then letter else '-' | letter <- word]
