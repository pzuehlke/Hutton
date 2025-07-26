-------------------------------------------------------
--  Exercise 11.2 - Programming in Haskell - Hutton  --
-------------------------------------------------------

import Data.Char
import Data.List
import System.IO
import System.Random (randomRIO)

-- The function `bestMove` needs to be redefined. The idea is simple: instead
-- of choosing the head of the list of grids that have the best label, we just
-- choose a random index and extract the corresponding grid. However, because
-- random number generation is impure, we need to modify the return type to
-- IO Grid and use `do` notation.
bestMove :: Grid -> Player -> IO Grid
bestMove grid player = do
    random <- randomRIO (0, length grids - 1)
    return (grids !! random) where
        grids = [grid' | Node (grid', label) _ <- children, label == best]
        tree = prune depth (gameTree grid player)
        Node (_, best) children = minimax tree

-- Because of the change of the type, we also need to modify the last line of
-- play':
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
        grid' <- bestMove grid player   -- new
        play grid' (next player)        -- new

-- The rest of the code is exactly as in the text.

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
-- full = all (all (/= B))

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

-- Minimax

minimax :: Tree Grid -> Tree (Grid, Player)
minimax (Node grid [])
    | wins O grid   = Node (grid, O) []
    | wins X grid   = Node (grid, X) []
    | otherwise     = Node (grid, B) []
minimax (Node grid children)
    | turn grid == O = Node (grid, minimum labels) children'
    | turn grid == X = Node (grid, maximum labels) children' where
        children' = map minimax children
        labels    = [player | Node (_, player) _ <- children']

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

prompt :: Player -> String
prompt player = "Player " ++ show player ++ ", enter your move: "
