-------------------------------------------------------
--  Exercise 11.3 - Programming in Haskell - Hutton  --
-------------------------------------------------------

import Data.Char
import Data.List
import System.IO

-- Basic declarations

size :: Int
size = 3

data Player = O | B | X deriving (Eq, Ord, Show)

type Grid = [[Player]]

next :: Player -> Player
next O = X
next X = O
next B = B

-- Grid utilities

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
turn grid = if os <= xs then O else X where
    os = length (filter (== O) squares)
    xs = length (filter (== X) squares)
    squares = concat grid

wins :: Player -> Grid -> Bool
wins player grid = any (all (== player)) lines where
    lines = horizontals ++ verticals ++ diagonals
    diagonals   = [diagonal grid, diagonal (map reverse grid)]
    horizontals = grid
    verticals   = transpose grid

diagonal :: Grid -> [Player]
diagonal grid = [(grid !! i) !! i | i <- [0..(size - 1)]]

won :: Grid -> Bool
won grid = wins O grid || wins X grid

-- Displaying a grid

interleave :: a -> [a] -> [a]
interleave _ []     = []
interleave _ [y]    = [y]
interleave x (y:ys) = y : x : (interleave x ys)

showPlayer :: Player -> [String]
showPlayer O = ["   ", " O ", "   "]
showPlayer B = ["   ", "   ", "   "]
showPlayer X = ["   ", " X ", "   "]

showRow :: [Player] -> [String]
showRow = beside . interleave bar . map showPlayer where
    beside = foldr1 (zipWith (++))
    bar = replicate 3 "|"

putGrid :: Grid -> IO ()
putGrid = putStrLn . unlines . concat . interleave bar . map showRow where
    bar = [replicate ((size * 4) - 1) '-']

-- Making a move

valid :: Grid -> Int -> Bool
valid grid i = 0 <= i && i < size^2 && concat grid !! i == B

move :: Grid -> Int -> Player -> [Grid]
move grid i p =
    if valid grid i then [chop size (xs ++ [p] ++ ys)] else [] where
        (xs, B:ys) = splitAt i (concat grid)

chop :: Int -> [a] -> [[a]]
chop n [] = []
chop n xs = take n xs : chop n (drop n xs)

-- Reading a number

getNat :: String -> IO Int
getNat prompt = do
    putStr prompt
    xs <- getLine
    if xs /= "" && all isDigit xs
        then return (read xs)
        else do
            putStrLn "ERROR: Invalid number"
            getNat prompt

-- Game trees

data Tree a = Node a [Tree a] deriving Show

gameTree :: Grid -> Player -> Tree Grid
gameTree grid player = Node grid [gameTree grid' (next player) |
                                  grid' <- moves grid player]

moves :: Grid -> Player -> [Grid]
moves grid player
    | won  grid     = []
    | full grid     = []
    | otherwise     = concat [move grid i player | i <- [0..(size^2 - 1)]]

-- Pruning the tree

prune :: Int -> Tree a -> Tree a
prune 0 (Node x _)          = Node x []
prune n (Node x children)   = Node x [prune (n - 1) tree | tree <- children]

depth :: Int
depth = 9

-- Minimax with depth - returns (Grid, Player, Depth)
minimax :: Tree Grid -> Tree (Grid, Player, Int)
minimax (Node grid [])
    | wins O grid       = Node (grid, O, 0) []
    | wins X grid       = Node (grid, X, 0) []
    | otherwise         = Node (grid, B, 0) []
minimax (Node grid children)
    | turn grid == O    = Node (grid, O, minDepth + 1) children'
    | turn grid == X    = Node (grid, X, minDepth + 1) children'
    where
        children' = map minimax children
        winningDepths = [d | Node (_, turn grid, d) _ <- children']
        minDepth = minimum winningDepths
-- We are using that (Player, Int) is already ordered lexicographically

bestMove :: Grid -> Player -> Grid
bestMove grid player =
    head [grid' | Node (grid', label, d) _ <- children, (label, d) == best]
    where
        tree = prune depth (gameTree grid player)
        Node (_, bestLabel, bestDepth) children = minimax tree
        best = (bestLabel, bestDepth)

-- Playing the game

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    play empty O

play :: Grid -> Player -> IO ()
play grid player = do
    cls
    goto (1, 1)
    putGrid grid
    play' grid player

play' :: Grid -> Player -> IO ()
play' grid player
    | wins O grid   = putStrLn "Player O wins!\n"
    | wins X grid   = putStrLn "Player X wins!\n"
    | full grid     = putStrLn "It's a draw!\n"
    | player == O   = do
        i <- getNat (prompt player)
        case move grid i player of
            [] -> do
                putStrLn "ERROR: Invalid move"
                play' grid player
            [grid'] -> play grid' (next player)
    | player == X   = do
        putStr "Player X is thinking... "
        play (bestMove grid player) (next player)

prompt :: Player -> String
prompt player = "Player " ++ show player ++ ", enter your move: "
