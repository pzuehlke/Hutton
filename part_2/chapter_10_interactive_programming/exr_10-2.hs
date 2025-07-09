------------------------------------------------------
--  Exercise 10.2 - Programming in Haskell - Hutton  --
------------------------------------------------------

type Board = [Int]

putBoard :: Board -> IO ()
putBoard board = putBoardIter board 1

putBoardIter :: Board -> Int -> IO ()
putBoardIter [] _                   = return ()
putBoardIter (num_stars:ns) row     = do
    putStr (show row)
    putStr ":"
    putStrLn (concat (replicate num_stars " *"))
    putBoardIter ns (row + 1)
    return ()
