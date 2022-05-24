----------------------------------------------------
--  CHAPTER 10 - PROGRAMMING IN HASKELL - HUTTON  --
----------------------------------------------------

act :: IO (Char, Char)
act = do x <- getChar
         getChar
         y <- getChar
         return (x, y)

myGetLine :: IO String
myGetLine = do x <- getChar
               if x == '\n' then
                            return []
               else
                  do xs <- myGetLine
                     return (x:xs)

myPutStr :: String -> IO ()
myPutStr []     = return ()
myPutStr (c:cs) = do putChar c
                     myPutStr cs

myPutStrLn :: String -> IO ()
myPutStrLn str = do putStr str
                    putChar '\n'

strlen :: IO ()
strlen = do putStr "Enter a string: "
            xs <- getLine
            putStr "The string has "
            putStr (show (length xs))
            putStrLn " characters."
