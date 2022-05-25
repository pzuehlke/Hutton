------------------------------------------------------
--  Exercise 10.3 - Programming in Haskell - Hutton  --
------------------------------------------------------

type Board = [Int]

putRow :: Int -> Int -> IO ()
putRow row num = do putStr (show row)
                    putStr ": "
                    putStrLn (replicate num '*')

putBoard :: Board -> IO ()
putBoard board = sequence_ [putRow row num | (row, num) <- zip [1..] board]
