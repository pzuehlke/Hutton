------------------------------------------------------
--  Exercise 10.6 - Programming in Haskell - Hutton  --
------------------------------------------------------

import System.IO

getCh :: IO Char
getCh = do
    hSetEcho stdin False
    char <- getChar
    hSetEcho stdin True
    return char

readLine :: IO String
readLine = do
    c <- getCh
    case c of
        '\n' -> do
            putChar c
            return ""
        '\DEL' -> do
            putStr "\b \b"
            -- Note that '\b' is a _non-destructive_ backspace, hence the
            -- string "\b \b" moves one character leftward, replaces it by
            -- a space, and then moves leftward again to await a new input.
            restOfLine <- readLine
            return ('\DEL':restOfLine)
        _ ->  do
            putChar c
            text <- readLine
            return (case text of
                [] -> [c]
                ('\DEL':rest) -> rest   -- c was deleted, shouldn't be returned
                otherwise -> (c:text)
                )
