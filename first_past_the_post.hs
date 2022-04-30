------------------------------------------------------------------
--  Implementation of a "first past the post" voting algorithm  --
------------------------------------------------------------------

import Data.List

-- Sample list of votes.
votes :: [String]
votes = ["Red", "Blue", "Green", "Blue", "Blue", "Red", "Yellow", "Red"]

-- | Count the number of times an item occurs in a list.
count :: Eq a => a -> [a] -> Int
count x = length . filter (== x)

-- | Given a list, delete all duplicates occurring in it so that each of the
-- original elements appears exactly once.
removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates []     = []
removeDuplicates (x:xs) = x : filter (/= x) (removeDuplicates xs)

-- | Determine the maximum number of votes received by a candidate.
maxVotes :: Eq a => [a] -> Int
maxVotes vs = (last . sort) [count v vs | v <- removeDuplicates vs]

-- | Determine the winner(s) of the election.
winner :: Ord a => [a] -> [a]
winner vs = sort [v | v <- removeDuplicates vs, count v vs == maxVotes vs]
