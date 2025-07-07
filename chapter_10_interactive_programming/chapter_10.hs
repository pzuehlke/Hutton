----------------------------------------------------
--  CHAPTER 10 - PROGRAMMING IN HASKELL - HUTTON  --
----------------------------------------------------

{--
getChar :: IO Char
getChar = ...

putChar :: Char -> IO ()
putChar c = ...

return :: a -> IO a
return v = ...

do  v1 <- a1
    v2 <- a2
    .
    .
    .
    vn <- an
    return (f v1 v2 ... vn)
--}

-- | Reads two characters, ignoring the middle one, and returns them as a pair.
act :: IO (Char, Char)
act = do c <- getChar
         getChar
         d <- getChar
         return (c, d)

-- | Reads a line of input from stdin until a newline character is encountered.
getLine' :: IO String
getLine' = do
    char <- getChar
    if char == '\n'
        then return []
        else do
            rest <- getLine'
            return (char:rest)

-- | Outputs a string to stdout without adding a newline.
putStr' :: String -> IO ()
putStr' []  = return ()
putStr' (char:rest) = do
    putChar char
    putStr' rest

-- | Outputs a string to stdout followed by a newline character.
putStrLn' :: String -> IO ()
putStrLn' text = do
    putStr' text
    putChar '\n'

-- | Prompts the user for a string and displays its character count.
strlen :: IO ()
strlen = do
    putStr "Enter a string: "
    text <- getLine
    putStr "The string has "
    putStr (show (length text))
    putStrLn' " characters."
