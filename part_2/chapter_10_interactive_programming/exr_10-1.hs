-------------------------------------------------------
--  Exercise 10.1 - Programming in Haskell - Hutton  --
-------------------------------------------------------

myPutStr :: String -> IO ()
myPutStr string = sequence_ [putChar c | c <- string]
