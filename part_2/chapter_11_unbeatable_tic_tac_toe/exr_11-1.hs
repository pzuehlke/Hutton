-------------------------------------------------------
--  Exercise 11.1 - Programming in Haskell - Hutton  --
-------------------------------------------------------

import Data.List

data Tree a = Node a [Tree a] deriving Show

-- New code:
countNodes :: Tree a -> Int
countNodes (Node _ children) = 1 + (sum . map countNodes) children

depth :: Tree a -> Int
depth (Node _ [])       = 0  -- necessary because maximum expects nonempty list
depth (Node _ children) = 1 + (maximum . map depth) children

-- Running countNodes (gameTree empty O) returns 549 496 as in the statement.
-- Running depth (gameTree empty O) returns 9, also as expected.
-- The rest of the code is as in the text, but I removed anything that was
-- unnecessary to solve the problem.

-- Basic declarations

size :: Int
size = 3

data Player = O | B | X deriving (Eq, Ord, Show)

type Grid = [[Player]]

next :: Player -> Player
next O = X
next X = O
next B = B

empty :: Grid
empty = replicate size (replicate size B)

full :: Grid -> Bool
full = all (/= B) . concat

wins :: Player -> Grid -> Bool
wins player grid = any (all (== player)) lines where
    lines = horizontals ++ verticals ++ diagonals
    diagonals   = [diag grid, diag (map reverse grid)]
    horizontals = grid
    verticals   = transpose grid

diag :: Grid -> [Player]
diag grid = [grid !! n !! n | n <- [0..(size - 1)]]

won :: Grid -> Bool
won grid = wins O grid || wins X grid

valid :: Grid -> Int -> Bool
valid grid i = 0 <= i && i < size^2 && concat grid !! i == B

move :: Grid -> Int -> Player -> [Grid]
move grid i p =
    if valid grid i then [chop size (xs ++ [p] ++ ys)] else [] where
        (xs, B:ys) = splitAt i (concat grid)

chop :: Int -> [a] -> [[a]]
chop n [] = []
chop n xs = take n xs : chop n (drop n xs)

moves :: Grid -> Player -> [Grid]
moves grid player
    | won  grid     = []
    | full grid     = []
    | otherwise     = concat [move grid i player | i <- [0..(size^2 - 1)]]

gameTree :: Grid -> Player -> Tree Grid
gameTree grid player = Node grid [gameTree grid' (next player) |
                                  grid' <- moves grid player]
