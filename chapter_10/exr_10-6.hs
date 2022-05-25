------------------------------------------------------
--  Exercise 10.6 - Programming in Haskell - Hutton  --
------------------------------------------------------

import System.IO

getCh :: IO Char
getCh = do hSetEcho stdin False
           x <- getChar
           hSetEcho stdin True
           return x

readLine :: IO String
readLine = do c <- getCh
              if c == '\n' then
                do putChar c
                   return ""
              else if c == '\DEL' then
                do putStr "\b \b"
-- Note that '\b' is a _non-destructive_ backspace, hence the string "\b \b"
-- moves one character leftward, replaces it by a space, and then moves
-- leftward again to await a new input.
                   cs <- readLine
                   return ('\DEL':cs)
              else
                do putChar c
                   cs <- readLine
                   return (case cs of
                             [] -> [c]
                             ('\DEL':ds) -> ds
                             otherwise -> (c:cs)
                          )
