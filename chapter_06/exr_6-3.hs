------------------------------------------------------
--  EXERCISE 6.3 - PROGRAMMING IN HASKELL - HUTTON  --
------------------------------------------------------

exp :: Int -> Int -> Int
exp a 0     = 1
exp a n     = a * (exp a (n - 1))


-- Calling exp 2 3:
-- exp 2 3
-- =    {applying exp}
--   2 * (exp 2 2)
-- =    {applying exp}
--   2 * (2 * (exp 2 1))
-- =    {applying exp}
--   2 * (2 * (2 * (exp 2 0))
-- =    {applying exp}
--   2 * (2 * (2 * 1))
-- =    {applying * several times}
--   8
