-------------------------------------------------------
--  Exercise 15.2 - Programming in Haskell - Hutton  --
-------------------------------------------------------

-- Using outermost evaluation in fst (1 + 2, 2 + 3), the calculation proceeds
-- as follows:
-- fst (1 + 2, 2 + 3) -> 1 + 2 {application of fst} -> 3 {application of (+)}
--
-- Using innermost evaluation, it proceeds as follows:
-- fst (1 + 2, 2 + 3) -> fst (3, 2 + 3) {evaluation of 1 + 2}
--      -> fst (3, 5) {evaluation of 2 + 3} -> 3 {application of fst}
-- 
-- Thus, using outermost evaluation one step is avoided.
