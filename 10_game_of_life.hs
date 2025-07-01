--------------------
--  Game of Life  --
--------------------

clearScreen :: IO ()
clearScreen = putStr "\ESC[2J"

type Pos = (Int, Int)

writeAt :: Pos -> String -> IO ()
writeAt position text = do
    goto position
    putStr text

goto :: Pos -> IO ()
goto (x, y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

width :: Int
width = 10

height :: Int
height = 10

type Board = [Pos]  -- Actually will be used to represent the living cells

glider :: Board
glider = [(4, 2), (2, 3), (4, 3), (3, 4), (4, 4)]

showCells :: Board -> IO ()
showCells board = sequence_ [writeAt position "O" | position <- board]

isAlive :: Board -> Pos -> Bool
isAlive board position = elem position board

isEmpty :: Board -> Pos -> Bool
isEmpty board position = not (isAlive board position)

neighbors :: Pos -> [Pos]
neighbors (x, y) = map wrap [(x - 1, y - 1), (x, y - 1), (x + 1, y - 1),
                             (x - 1, y),                 (x + 1, y),
                             (x - 1, y + 1), (x, y + 1), (x + 1, y + 1)]

wrap :: Pos -> Pos
wrap (x, y) = (((x - 1) `mod` width) + 1,
               ((y - 1) `mod` height) + 1)

liveNeighbors :: Board -> Pos -> Int
liveNeighbors board = length . filter (isAlive board) . neighbors

survivors :: Board -> [Pos]
survivors board = [position | position <- board,
                              elem (liveNeighbors board position) [2, 3]]

births :: Board -> [Pos]
births board =
    [position | position <- removeDuplicates (concat (map neighbors board)),
                isEmpty board position, liveNeighbors board position == 3]

removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates []     = []
removeDuplicates (x:xs) = x : removeDuplicates (filter (/= x) xs)

nextGeneration :: Board -> Board
nextGeneration board = survivors board ++ births board

life :: Board -> IO ()
life board = do
    clearScreen
    showCells board
    wait 500000
    life (nextGeneration board)

wait :: Int -> IO ()
wait n = sequence_ [return () | _ <- [1..n]]
