------------------------------------------------------
--  EXERCISE 2.3 - PROGRAMMING IN HASKELL - HUTTON  --
------------------------------------------------------

-- The three errors are:
--      * 'N' is not a valid variable/function name because it begins with an
--      uppercase letter;
--      * The quotes surrounding "div" should be backward, not forward quotes;
--      * 'a' and the 'x' in 'xs' should be aligned to the same column;
--      * Also, to be consistent with what is written on p. 17, there should be
--      a pair of parentheses surrounding "length xs", to prevent GHCi from
--      trying to evaluate 'div' on both 'length' and 'xs'. However, this does
--      not occur in practice, probably because "length xs" gets evaluated
--      first.
--
-- Incorporating these corrections, we have:
n = a `div` (length xs)
    where
        a = 10
        xs = [1, 2, 3, 4, 5]
        
