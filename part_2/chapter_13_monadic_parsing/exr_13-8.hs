-------------------------------------------------------
--  Exercise 13.8 - Programming in Haskell - Hutton  --
-------------------------------------------------------

-- (a) Following the pattern of the grammar on p. 189:
-- expr       := subtrahend | expr - subtrahend
-- subtrahend := (expr) | nat
-- nat        := 0 | 1 | 2 | ...
--
-- In abbreviated form, we could also write:
-- expr       := (expr - | eps) subtrahend
--
-- It is easy to check that the resulting subtraction is indeed
-- left-associative. Or if we don't want to allow parentheses, we could
-- simplify the grammar to have only two rules:
-- expr := nat | expr - nat
-- nat  := 0 | 1 | 2 | ...

-- (b) We need one parser for each of `expr` and `subtrahend`:
-- expr       := subtrahend | expr - subtrahend
expr :: Parser Int
expr = do
    s <- subtrahend
    return s
  <|> do
    e <- expr
    symbol "-"
    s <- subtrahend
    return (e - s)

-- subtrahend := (expr) | nat
subtrahend :: Parser Int
subtrahend = do
    symbol "("
    e <- expr
    symbol ")"
    return e
  <|> natural

-- (c) There is a problem with `expr`. Consider trying to parse an expression
-- such as `4 - 2`. It will parse 4 as a subtrahend/nat, but then will fail to
-- parse `- 2` under both alternatives.
--
-- It also doesn't help to try to reformulate the rule in the equivalent form
-- expr := expr - subtrahend | subtrahend
-- The first thing the corresponding parser will do is to call itself
-- recursively, which will always lead to an infinite loop. 
--
-- Note that the grammars are all correct mathematically, but there is a
-- problem with the code.
--
-- (d) We need to modify the grammar rules, for instance:
-- expr         := minuend subtrahend
-- subtrahend   := [- minuend]* 
--      (where * denotes 0 or more repetitions and '[', ']' are _not_ symbols)
-- minuend   := (expr) | nat
-- nat          := 0 | 1 | 2 | ...
--
-- More explicitly, we could rewrite the rule for `subtrahend` as:
-- subtrahend   := - minuend subtrahend | eps
-- 
-- We begin with a direct implementation (without using `many` or `foldl`).

-- expr := minuend subtrahend
expr :: Parser Int
expr = do
    m <- minuend
    s <- subtrahend
    return m + s    -- '+' because subtrahend already has a minus sign in front

-- subtrahend := - minuend subtrahend | eps
subtrahend :: Parser Int
subtrahend = do
    symbol "-"
    m <- minuend
    s <- subtrahend
    return -m + s
  <|> return 0

-- minuend   := (expr) | nat
minuend :: Parser Int
minuend = do
    symbol "("
    e <- expr
    symbol ")"
    return e
  <|> natural

-- Now we can easily translate the original implementation to use `many` and
-- `foldl` as requested:
-- expr := minuend (- minuend)*
expr :: Parser Int
expr = do
    first <- minuend
    rest  <- many (do
        symbol "-"
        minuend)
    return (foldl (-) first rest)
-- Recall that `many p` has type `Parser [a]` if p has type `Parser a`.
