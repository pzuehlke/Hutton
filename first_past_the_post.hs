------------------------------------------------------------------
--  Implementation of a "first past the post" voting algorithm  --
------------------------------------------------------------------
-- This is a modified version of the "first past the post" voting algorithm
-- explained in chapter 7 of _Programming in Haskell_ by G. Hutton. Unlike the
-- latter, this one also allows for the possibility of more than one winner.

import Data.List

-- Sample list of votes.
votes :: [String]
votes = ["Red", "Blue", "Green", "Blue", "Blue", "Red", "Yellow", "Red"]

-- | Counts the number of times an item occurs in a list.
count :: Eq a => a -> [a] -> Int
count x = length . filter (== x)

-- | Given a list, deletes all duplicates occurring in it so that each of the
-- original elements appears exactly once.
removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates []     = []
removeDuplicates (x:xs) = x : filter (/= x) (removeDuplicates xs)

-- | Determines the maximum number of votes received by a candidate.
maxVotes :: Eq a => [a] -> Int
maxVotes vs = (last . sort) [count v vs | v <- removeDuplicates vs]

-- | Determines the winner(s) of the election.
winner :: Ord a => [a] -> [a]
winner vs = sort [v | v <- removeDuplicates vs, count v vs == maxVotes vs]

-- Sample test:
-- winner votes
