--------------------
--  Game of Life  --
--------------------

cls :: IO ()
cls = putStr "\ESC[2J"

type Pos = (Int, Int)

writeAt :: Pos -> String -> IO ()
writeAt p xs = do goto p
                  putStr xs

goto :: Pos -> IO ()
goto (x, y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

width :: Int
width = 10

height :: Int
height = 10

type Board = [Pos]

glider :: Board
glider = [(4, 2), (2, 3), (4, 3), (3, 4), (4, 4)]

showCells :: Board -> IO ()
showCells board = sequence_ [writeAt pos "O" | pos <- board]

isAlive :: Board -> Pos -> Bool
isAlive board pos = elem pos board

isEmpty :: Board -> Pos -> Bool
isEmpty board pos = not (isAlive board pos)

neighbors :: Pos -> [Pos]
neighbors (x, y) = map wrap [(x - 1, y - 1), (x, y - 1), (x + 1, y - 1),
                             (x - 1, y),                 (x + 1, y),
                             (x - 1, y + 1), (x, y + 1), (x + 1, y + 1)]

wrap :: Pos -> Pos
wrap (x, y) = (((x - 1) `mod` width) + 1, ((y - 1) `mod` height) + 1)

liveNeighbors :: Board -> Pos -> Int
liveNeighbors board = length . filter (isAlive board) . neighbors

survivors :: Board -> [Pos]
survivors board = [pos | pos <- board, elem (liveNeighbors board pos) [2, 3]]

births :: Board -> [Pos]
births board = [(x, y) | x <- [1..width], y <- [1..height],
                         isEmpty board (x, y),
                         liveNeighbors board (x, y) == 3]

births' :: Board -> [Pos]
births' board = [pos | pos <- rmdups (concat (map neighbors board)),
                       isEmpty board pos,
                       liveNeighbors board pos == 3]

rmdups :: Eq a => [a] -> [a]
rmdups []       = []
rmdups (x:xs)   = x : rmdups (filter (/= x) xs)

nextGen :: Board -> Board
nextGen board = survivors board ++ births board

life :: Board -> IO ()
life board = do cls
                showCells board
                wait 500000
                life (nextGen board)

wait :: Int -> IO ()
wait n = sequence_ [return () | _ <- [1..n]]
