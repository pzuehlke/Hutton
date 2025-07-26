-------------------------------------------------------
--  Exercise 13.1 - Programming in Haskell - Hutton  --
-------------------------------------------------------
import Control.Applicative
import Data.Char

newtype Parser a = P (String -> [(a, String)])

comment :: Parser ()
comment = do
    string "--"
    many (sat (/= '\n'))
    char '\n'
    return ()

-- Sample tests:
test1, test2 :: [((), String)]
test1 = parse comment "-- comment\nremaining text"
test2 = parse comment "not -a- comment"

parse :: Parser a -> String -> [(a, String)]
parse (P p) input = p input

item :: Parser Char
item = P (\input -> case input of
                        []      -> []
                        (x:xs)  -> [(x, xs)])

instance Functor Parser where
    -- fmap :: (a -> b) -> Parser a -> Parser b
    fmap f p = P (\input -> case parse p input of
                                [(v, out)]  -> [(f v, out)]
                                []          -> [])

instance Applicative Parser where
    -- pure :: a -> Parser a
    pure v = P (\input -> [(v, input)])

    -- (<*>) :: Parser (a -> b) -> Parser a -> Parser b
    pf <*> px = P (\input -> case parse pf input of
                                 [(f, out)] -> parse (fmap f px) out
                                 []         -> [])

instance Monad Parser where
    -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    p >>= f = P (\input -> case parse p input of
                               [(v, out)]   -> parse (f v) out
                               []           -> [])

instance Alternative Parser where
    -- empty :: Parser a
    empty = P (\input -> [])

    -- (<|>) :: Parser a -> Parser a -> Parser a
    p <|> q = P (\input -> case parse p input of
                               []           -> parse q input
                               [(v, out)]   -> [(v, out)])

sat :: (Char -> Bool) -> Parser Char
sat p = do
    x <- item
    if p x then return x else empty

char :: Char -> Parser Char
char x = sat (== x)

string :: String -> Parser String
string []       = return []
string (x:xs)   = do
    char x
    string xs
    return (x:xs)
