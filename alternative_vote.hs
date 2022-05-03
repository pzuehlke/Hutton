-----------------------------------------------------
--  Implementation of the alternative vote system  --
-----------------------------------------------------
-- This is a modified version of the alternative voting system explained in 
-- chapter 7 of _Programming in Haskell_ by G. Hutton. Unlike the latter, this
-- one also allows for the possibility of more than one winner.

import Data.List

-- Sample list of ballots. Each person may vote in as many distinct candidates
-- as they want, in order of decreasing preference.
ballots :: [[String]]
ballots = [["Red", "Green"]
          ,["Blue"]
          ,["Green", "Red", "Blue"]
          ,["Blue", "Green", "Red"]
          ,["Green", "Blue", "Red"]
          ,["Blue", "Green", "Red"]
          ,["Purple", "Yellow"]
          ,["Orange", "Yellow"]
          ,["Gray", "Yellow"]]

-- | Given a list, delete all duplicates occurring in it, so that each of the
-- original elements appears exactly once.
removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates []     = []
removeDuplicates (x:xs) = x : filter (/= x) (removeDuplicates xs)

-- | Counts the number of times an item occurs in a list.
count :: Eq a => a -> [a] -> Int
count x = length . filter (== x)

-- | Removes any empty ballots.
removeEmpty :: Eq a => [[a]] -> [[a]]
removeEmpty = filter (/= [])

-- | Eliminates an element from each list in a list of lists, if present.
eliminate :: Eq a => a -> [[a]] -> [[a]]
eliminate x = map (filter (/= x))

-- | Given a list, returns the least number of times that one of its elements
-- occurs in it. For example:
-- minVotes [1, 1, 2, 2, 2, 3, 3] = 2.
minVotes :: Eq a => [a] -> Int
minVotes vs = (head . sort) [count v vs | v <- vs]

-- | Given a list of lists, returns a list consisting of the elements occurring
-- in the 0-th position of each of the original lists.
firstChoices :: [[a]] -> [a]
firstChoices = map head

-- | Given a list of ballots, determines the loser(s) of the present round.
-- More explicitly, it filters from the ballots all of those candidates which
-- received the least number of "first choice" votes, along with all
-- candidates which received no "first choice" votes at all.
losers :: Ord a => [[a]] -> [a]
losers bs = sort [v | v <- cands, count v firsts <= minv]
            where
              cands = removeDuplicates (concat bs)
              firsts = firstChoices bs
              minv = minVotes firsts

-- | Decides whether a list contains some element.
notElem' :: Eq a => [a] -> a -> Bool
notElem' xs x = not (elem x xs)

-- | Given a ballot, returns a new ballot by removing all losers of the present
-- round (i.e., those who received no "first choice" votes or who received the
-- least number of such votes).
nextRound :: Ord a => [[a]] -> [[a]]
nextRound bs = (removeEmpty . map (filter (notElem' (losers bs)))) bs

-- | Given a list of lists, return the set of all elements which occur in at
-- least one of them (in other words, construct the set union of all lists).
occur :: Eq a => [[a]] -> [a]
occur = removeDuplicates . concat

-- | Determines the winner(s) of the election.
winners :: Ord a => [[a]] -> [a]
winners bs  | nextRound bs /= []    = winners (nextRound bs)
            | otherwise             = occur bs
