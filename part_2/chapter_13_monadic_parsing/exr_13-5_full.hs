-------------------------------------------------------
--  Exercise 13.5 - Programming in Haskell - Hutton  --
-------------------------------------------------------

-- Here is the full code. There are three sample expressions at the end for
-- testing.

import Control.Applicative
import Data.Char

newtype Parser a = P (String -> [(a, String)])

parse :: Parser a -> String -> [(a, String)]
parse (P p) input = p input

item :: Parser Char
item = P (\input -> case input of
                        ""      -> []
                        (c:cs)  -> [(c, cs)])

instance Functor Parser where
    fmap f p = P (\input -> case parse p input of
                                [(v, out)]  -> [(f v, out)]
                                []          -> [])

instance Applicative Parser where
    pure v = P (\input -> [(v, input)])
    pf <*> px = P (\input -> case parse pf input of
                                 [(f, out)] -> parse (fmap f px) out
                                 []         -> [])

instance Monad Parser where
    p >>= f = P (\input -> case parse p input of
                               [(v, out)]   -> parse (f v) out
                               []           -> [])

instance Alternative Parser where
    empty = P (\input -> [])
    p <|> q = P (\input -> case parse p input of
                               []           -> parse q input
                               [(v, out)]   -> [(v, out)])

sat :: (Char -> Bool) -> Parser Char
sat p = do
    c <- item
    if p c then return c else empty

digit :: Parser Char
digit = sat isDigit

char :: Char -> Parser Char
char c = sat (== c)

string :: String -> Parser String
string ""       = return []
string (c:cs)   = do
    char c
    string cs
    return (c:cs)

space :: Parser ()
space = do
    many (sat isSpace)
    return ()

token :: Parser a -> Parser a
token p = do
    space
    v <- p
    space
    return v

symbol :: String -> Parser String
symbol xs = token (string xs)

-- Expression data type
data Expr = Add Expr Expr | Mul Expr Expr | Par Expr | Val Int
    deriving (Show, Eq)

-- Expression parser
expr :: Parser Expr
expr = do
    t <- term
    do
        symbol "+"
        e <- expr
        return (Add t e)
      <|> return t

term :: Parser Expr
term = do
    f <- factor
    do
        symbol "*"
        t <- term
        return (Mul f t)
      <|> return f

factor :: Parser Expr
factor = do
    symbol "("
    e <- expr
    symbol ")"
    return (Par e)
  <|> int

nat :: Parser Int
nat = do
    ds <- some digit
    return (read ds)

int :: Parser Expr
int = do
    n <- nat
    return (Val n)
  <|> do
    char '-'
    n <- nat
    return (Val (-n))

e1 :: [(Expr, String)]
e1 = parse expr "2 + 3"
-- Expected: [(Add (Val 2) (Val 3), "")]

e2 :: [(Expr, String)]
e2 = parse expr "(4 + 5) * 2"
-- Expected: [(Mul (Par (Add (Val 4) (Val 5))) (Val 2), "")]

e3 :: [(Expr, String)]
e3 = parse expr "-10 + 3 * (2 + 4)"
-- Expected: [(Add (Val (-10)) (Mul (Val 3) (Par (Add (Val 2) (Val 4)))), "")]
