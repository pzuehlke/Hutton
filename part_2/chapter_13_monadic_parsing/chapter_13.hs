import Control.Applicative
import Data.Char

newtype Parser a = P (String -> [(a, String)])

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

digit :: Parser Char
digit = sat isDigit

lower :: Parser Char
lower = sat isLower

upper :: Parser Char
upper = sat isUpper

letter :: Parser Char
letter = sat isAlpha

alphaNum :: Parser Char
alphaNum = sat isAlphaNum

char :: Char -> Parser Char
char x = sat (== x)

string :: String -> Parser String
string []       = return []
string (x:xs)   = do
    char x
    string xs
    return (x:xs)

ident :: Parser String
ident = do
    x  <- lower
    xs <- many alphaNum
    return (x:xs)

nat :: Parser Int
nat = do
    xs <- some digit
    return (read xs)

space :: Parser ()
space = do
    many (sat isSpace)
    return ()

int :: Parser Int
int = do
        char '-'
        n <- nat
        return (-n)
    <|> nat

token :: Parser a -> Parser a
token p = do
    space
    v <- p
    space
    return v

identifier :: Parser String
identifier = token ident

natural :: Parser Int
natural = token nat

integer :: Parser Int
integer = token int

symbol :: String -> Parser String
symbol xs = token (string xs)

nats :: Parser [Int]
nats = do
    symbol "["
    n<- natural
    ns <- many (do
        symbol ","
        natural)
    symbol "]"
    return (n:ns)

expr :: Parser Int
expr = do
    t <- term
    do
        symbol "+"
        e <- expr
        return (t + e)
      <|> return t

term :: Parser Int
term = do
    f <- factor
    do
        symbol "*"
        t <- term
        return (f * t)
      <|> return f

factor :: Parser Int
factor = do
    symbol "("
    e <- expr
    symbol ")"
    return e
  <|> natural

eval :: String -> Int
eval xs = case (parse expr xs) of
              [(n, [])]     -> n
              [(_, out)]    -> error ("Unused input " ++ out)
              []            -> error "Invalid input"

-- Calculator
box :: [String]
box = ["+---------------+",
       "|               |",
       "+---+---+---+---+",
       "| q | c | d | = |",
       "+---+---+---+---+",
       "| 1 | 2 | 3 | + |",
       "+---+---+---+---+",
       "| 4 | 5 | 6 | - |",
       "+---+---+---+---+",
       "| 7 | 8 | 9 | * |",
       "+---+---+---+---+",
       "| 0 | ( | ) | / |",
       "+---+---+---+---+"]

buttons :: String
buttons = standard ++ extra where
    standard = "qcd=123+456-789*0()/^"
    extra = "QCD\ESC\BS\DEL\n"

showBox :: IO ()
showBox = sequence_ [writeat (1, y) b | (y, b) <- zip [1..] box]

display :: IO()
display xs = do
    writeat (3, 2) (replicate 13 ' ')
    writeat (3, 2) (reverse (take 13 (reverse xs)))

calc :: String -> IO ()
calc xs = do
    display xs
    c <- getCh
    if elem c buttons
        then process c xs
        else do
            beep
            calc xs

beep :: IO ()
beep = putStr "\BEL"

process :: Char -> String -> IO ()
process c xs
    | elem c "qQ\ESC"       = quit
    | elem c "dD\BS\DEL"    = delete xs
    | elem c "=\n"          = evaluate xs
    | elem c "cC"           = clear
    | otherwise             = press c xs

quit :: IO ()
quit = goto (1, 14)

delete :: String -> IO ()
delete [] = calc []
delete xs = calc (init xs)

evaluate :: String -> IO ()
evaluate xs = case parse expr xs of
                  [(n, [])] -> calc (show n)
                  _         -> do
                      beep
                      calc xs

clear :: IO ()
clear = calc []

press :: Char -> String -> IO ()
press c xs = calc (xs ++ [c])

run :: IO ()
run = do
    cls
    showbox
    clear
