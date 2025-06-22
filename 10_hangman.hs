------------------------------------------
--  Implementation of the game Hangman  --
------------------------------------------

import System.IO

hangman :: IO ()
hangman = do putStrLn "Think of a word: "
             word <- sgetLine
             putStrLn "Try to guess it or type 'quit' to exit: "
             play word

sgetLine :: IO String
sgetLine = do c <- getCh
              if c == '\n'
                 then
                 do putChar c
                    return []
              else
                 do putChar '-'
                    cs <- sgetLine
                    return (c:cs)

getCh :: IO Char
getCh = do hSetEcho stdin False
           x <- getChar
           hSetEcho stdin True
           return x

play :: String -> IO ()
play word = do putStr "> "
               guess <- getLine
               if guess == "quit" then return ()
               else if guess == word then
                  putStrLn "You got it !!"
               else
                  do putStrLn (match word guess) 
                     play word

match :: String -> String -> String
match xs ys = [if elem x ys then x else '-' | x <- xs]
