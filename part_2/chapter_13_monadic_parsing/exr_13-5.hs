-------------------------------------------------------
--  Exercise 13.5 - Programming in Haskell - Hutton  --
-------------------------------------------------------

-- Here are the parts that are actually different from those in the original.
-- The full implementation is provided in the separate file `13-5_full.hs`.
-- First, we need a new type. We follow closely the grammar on p. 189, this is
-- why we also include a constructor to represent a parenthesized expression.
data Expr = Add Expr Expr | Mul Expr Expr | Par Expr | Val Int
    deriving (Show, Eq)

-- Next we need to update the main parsers to return Expr values:
expr :: Parser Expr
expr = do
    t <- term
    do
        symbol "+"
        e <- expr
        return (Add t e)    -- Return (Add t e) instead of (t + e)
      <|> return t

term :: Parser Expr
term = do
    f <- factor
    do
        symbol "*"
        t <- term
        return (Mul f t)    -- Similarly
      <|> return f

factor :: Parser Expr
factor = do
    symbol "("
    e <- expr
    symbol ")"
    return (Par e)  -- Similarly
  <|> natural

-- The int parser is the one that needs to be modified the most:
int :: Parser Expr
int = do
    n <- nat
    return (Val n)
  <|>
    char '-'
    n <- nat
    return (Val (-n))
