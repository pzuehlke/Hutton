------------------------------------------------------
--  Exercise 10.2 - Programming in Haskell - Hutton  --
------------------------------------------------------

type Board = [Int]

putBoard :: Board -> IO ()
putBoard board = putBoardIter board 1

putBoardIter :: Board -> Int -> IO ()
putBoardIter [] _       = return ()
putBoardIter (n:ns) k   = do putStr (show k)
                             putStr ": "
                             putStrLn (replicate n '*')
                             putBoardIter ns (k + 1)
                             return ()
