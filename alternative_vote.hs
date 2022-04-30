-----------------------------------------------------
--  Implementation of the alternative vote system  --
-----------------------------------------------------

import Data.List

-- Sample list of ballots. Each person may vote in as many distinct candidates
-- as they want, in order of decreasing preference.
ballots :: [[String]]
ballots = [["Red", "Green"]
          ,["Blue"]
          ,["Green", "Red", "Blue"]
          ,["Blue", "Green", "Red"]
          ,["Green", "Blue", "Red"]
          ,["Blue", "Green", "Red"]]

-- | Count the number of times an item occurs in a list.
count :: Eq a => a -> [a] -> Int
count x = length . filter (== x)

removeEmpty :: Eq a => [[a]] -> [[a]]
removeEmpty = filter (/= [])

eliminate :: Eq a => a -> [[a]] -> [[a]]
eliminate x = map (filter (/= x))

minVotes :: Eq a => [a] -> Int
minVotes vs = (head . sort) [count v vs | v <- removeDuplicates vs]

firstChoices :: [[a]] -> [a]
firstChoices = map head

losers :: Ord a => [[a]] -> [a]
losers bs = sort [v | v <- removeDuplicates vs, count v vs == minVotes vs]
            where
              vs = firstChoices bs

notElem' :: Eq a => [a] -> a -> Bool
notElem' xs x = not (elem x xs)

nextRound :: Ord a => [[a]] -> [[a]]
nextRound bs = (removeEmpty. map (filter (notElem' (losers bs)))) bs

-- | Given a list, delete all duplicates occurring in it so that each of the
-- original elements appears exactly once.
removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates []     = []
removeDuplicates (x:xs) = x : filter (/= x) (removeDuplicates xs)

-- | Determine the maximum number of votes received by a candidate.
maxVotes :: Eq a => [a] -> Int
maxVotes vs = (last . sort) [count v vs | v <- removeDuplicates vs]

-- | Given a list of lists, return the set of all elements which occur in at
-- least one of them (in other words, construct the set union of all lists).
occur :: Eq a => [[a]] -> [a]
occur = removeDuplicates . concat

-- | Determine the winner(s) of the election.
winners :: Ord a => [[a]] -> [a]
winners bs  | nextRound bs /= []    = winners (nextRound bs)
            | otherwise             = occur bs
