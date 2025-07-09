-------------------
--  Tic Tac Toe  --
-------------------

import Data.Char
import Data.List
import System.IO

--------------------------
--  Basic declarations  --
--------------------------

size :: Int
size = 3

data Player = O | B | X
    deriving (Eq, Ord, Show)

type Grid = [[Player]]

next :: Player -> Player
next O = X
next X = O
next B = B

----------------------
--  Grid Utilities  --
----------------------

type Pos = (Int, Int)

cls :: IO ()
cls = putStr "\ESC[2J"

goto :: Pos -> IO ()
goto (x, y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

empty :: Grid
empty = replicate size (replicate size B)

full :: Grid -> Bool
full = all (/= B) . concat

turn :: Grid -> Player
turn grid = if os <= xs then O else X
  where
    os = length (filter (== O) squares)
    xs = length (filter (== X) squares)
    squares = concat grid

wins :: Player -> Grid -> Bool
wins player grid = any filled (rows ++ columns ++ diagonals)
  where
    filled = all (== player)
    rows = grid
    columns = transpose grid
    diagonals = [diag grid, diag (map reverse grid)]

diag :: Grid -> [Player]
diag grid = [grid !! n !! n | n <- [0..(size - 1)]]

won :: Grid -> Bool
won grid = wins O grid || wins X grid

-------------------------
--  Displaying a grid  --
-------------------------

interleave :: a -> [a] -> [a]
interleave _ []     = []
interleave _ [y]    = [y]
interleave x (y:ys) = y : x : (interleave x ys)

showPlayer :: Player -> [String]
showPlayer O = ["   ", " O ", "   "]
showPlayer B = ["   ", "   ", "   "]
showPlayer X = ["   ", " X ", "   "]

showRow :: [Player] -> [String]
showRow = beside . interleave bar . map showPlayer
  where
    beside = foldr1 (zipWith (++))
    bar = replicate 3 "|"

putGrid :: Grid -> IO ()
putGrid = putStrLn . unlines . concat . interleave bar . map showRow
  where
    bar = [replicate ((size * 4) - 1) '-']

---------------------
--  Making a move  --
---------------------

valid :: Grid -> Int -> Bool
valid grid i = 0 <= i && i < size^2 && concat grid !! i == B

move :: Grid -> Int -> Player -> [Grid]
move grid i p = if valid grid i then [chop size (xs ++ [p] ++ ys)] else []
  where (xs, B:ys) = splitAt i (concat grid)

chop :: Int -> [a] -> [[a]]
chop n [] = []
chop n xs = take n xs : chop n (drop n xs)

------------------------
--  Reading a number  --
------------------------

getNat :: String -> IO Int
getNat prompt = do putStr prompt
                   xs <- getLine
                   if xs /= [] && all isDigit xs then
                       return (read xs)
                   else
                       do putStrLn "ERROR: Invalid number"
                          getNat prompt


ticTacToe :: IO ()
ticTacToe = run empty O

run :: Grid -> Player -> IO ()
run grid player = do cls
                     goto (1, 1)
                     putGrid grid
                     run' grid player

run' :: Grid -> Player -> IO ()
run' grid player    | wins O grid = putStrLn "Player O wins!\n"
                    | wins X grid = putStrLn "Player X wins!\n"
                    | full grid   = putStrLn "It's a draw!\n"
                    | otherwise   = 
                        do i<- getNat (prompt player)
                           case move grid i player of
                             [] -> do putStrLn "ERROR: Invalid move"
                                      run' grid player
                             [grid'] -> run grid' (next player)

prompt :: Player -> String
prompt player = "Player " ++ show player ++ ", enter your move: "
