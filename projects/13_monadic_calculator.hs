--------------------------
--  Monadic Calculator  --
--------------------------
-- A basic calculator that supports all five basic arithmetic operations on
-- integers (+, -, *, / and ^). To use it, type `run` in ghci.
--
-- Here are the grammar rules for the parser:
-- expr     := term   (+ expr | - expr | eps)
-- term     := factor (* term | / term | eps)
-- factor   := power  (^ factor | eps)
-- power    := (expr) | int
-- int      := ... | -2 | -1 | 0 | 1 | 2 | ...

import Control.Applicative
import Data.Char
import System.IO

newtype Parser a = P (String -> [(a, String)])

parse :: Parser a -> String -> [(a, String)]
parse (P p) input = p input

item :: Parser Char
item = P (\input -> case input of
                        ""      -> []
                        (c:cs)  -> [(c, cs)])

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
    c <- item
    if p c then return c else empty

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
char c = sat (== c)

string :: String -> Parser String
string ""       = return []
string (c:cs)   = do
    char c
    string cs
    return (c:cs)

-- class Applicative F => Alternative F where
--  empty :: F a
--  (<|>) :: F a -> F a -> F a
--  many  :: F a -> F [a]
--  some  :: F a -> F [a]
--
--  many f = some f <|> pure []
--  some f = pure (:) <*> f <*> many f

ident :: Parser String
ident = do
    c  <- lower
    cs <- many alphaNum
    return (c:cs)

nat :: Parser Int
nat = do
    ds <- some digit
    return (read ds)

space :: Parser ()
space = do
    many (sat isSpace)
    return ()

int :: Parser Int
int = nat <|> do
    char '-'
    n <- nat
    return (-n)

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
    n  <- natural
    ns <- many (do
        symbol ","
        natural)
    symbol "]"
    return (n:ns)

-- Arithmetic expressions
-- We include the grammar rules again for guidance.
-- expr     := term   (+ expr | - expr | eps)
expr :: Parser Int
expr = do
    t <- term
    do
        symbol "+"
        e <- expr
        return (t + e)
      <|> do
          symbol "-"
          e <- expr
          return (t - e)
      <|> return t

-- term     := factor (* term | / term | eps)
term :: Parser Int
term = do
    f <- factor
    do
        symbol "*"
        t <- term
        return (f * t)
      <|> do
          symbol "/"
          t <- term  
          if t == 0
              then empty -- or: error "Division by zero!"
              else return (f `div` t)
      <|> return f

-- factor   := power  (^ factor | eps)
factor :: Parser Int
factor = do
    p <- power
    do
        symbol "^"
        f <- factor
        if f < 0    -- avoid exponentiating by a negative exponent
            then empty
            else return (p ^ f)
      <|> return p
        
-- power    := (expr) | int
power :: Parser Int
power = do
    symbol "("
    e <- expr
    symbol ")"
    return e
  <|> integer

eval :: String -> Int
eval xs = case (parse expr xs) of
              [(n, [])]     -> n
              [(_, out)]    -> error ("Unused input " ++ out)
              []            -> error "Invalid input"


-- Calculator
type Pos = (Int, Int)

cls :: IO ()
cls = putStr "\ESC[2J"

writeAt :: Pos -> String -> IO ()
writeAt position text = do
    goto position
    putStr text

goto :: Pos -> IO ()
goto (x, y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

getCh :: IO Char
getCh = do
    hSetEcho stdin False
    char <- getChar
    hSetEcho stdin True
    return char

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
showBox = sequence_ [writeAt (1, y) b | (y, b) <- zip [1..] box]

display :: String -> IO ()
display cs = do
    writeAt (3, 2) (replicate 13 ' ')
    writeAt (3, 2) (reverse (take 13 (reverse cs)))

calc :: String -> IO ()
calc cs = do
    display cs
    c <- getCh
    if elem c buttons
        then process c cs
        else do
            beep
            display ("Invalid: " ++ show c)
            wait 5000000
            display cs
            calc cs

wait :: Int -> IO ()
wait n = sequence_ [return () | _ <- [1..n]]

beep :: IO ()
beep = putStr "\BEL"

process :: Char -> String -> IO ()
process c cs
    | elem c "qQ\ESC"       = quit
    | elem c "dD\BS\DEL"    = delete cs
    | elem c "=\n"          = evaluate cs
    | elem c "cC"           = clear
    | otherwise             = press c cs

quit :: IO ()
quit = goto (1, 14)

delete :: String -> IO ()
delete "" = calc ""
delete cs = calc (init cs)

evaluate :: String -> IO ()
evaluate cs =
    case parse expr cs of
        [(n, [])] -> calc (show n)
        [(_, s)]  -> do
            beep
            display ("error: " ++ (take 6 s))
            wait 5000000
            calc cs

clear :: IO ()
clear = calc ""

press :: Char -> String -> IO ()
press c cs = calc (cs ++ [c])

run :: IO ()
run = do
    cls
    showBox
    clear
