-------------------------------------------------------
--  Exercise 17.1 - Programming in Haskell - Hutton  --
-------------------------------------------------------

import Control.Applicative ((<|>))

type Stack = [Maybe Int]

data Expr = Val Int
          | Add Expr Expr
          | Throw
          | Catch Expr Expr
          deriving Show

data Code = HALT
          | PUSH Int Code
          | ADD Code
          | THROW Code
          | CATCH Code

eval :: Expr -> Maybe Int
eval (Val n)     = Just n
eval (Add x y)   = do
    n <- eval x
    m <- eval y
    return (m + n)
eval Throw       = Nothing
eval (Catch x h) = (eval x) <|> (eval h)

exec :: Code -> Stack -> Stack
exec HALT       s               = s
exec (PUSH n c) s               = exec c (Just n : s)
exec (ADD c)    (my : mx : s)   = exec c (sum : s) where
    sum = do
        n <- my
        m <- mx
        return (n + m)
exec (THROW c)  s               = exec c (Nothing : s)
exec (CATCH c)  (mh : mx : s)   = exec c ((mx <|> mh) : s)

comp' :: Expr -> Code -> Code
comp' (Val n)     c = PUSH n c
comp' (Add x y)   c = comp' x (comp' y (ADD c))
comp' Throw       c = THROW c
comp' (Catch x h) c = comp' x (comp' h (CATCH c))

comp :: Expr -> Code
comp e = comp' e HALT

-- Tests:
e1 :: Expr  -- Just 5
e1 = Catch (Val 5) Throw

e2 :: Expr  -- Just 7
e2 = Catch Throw (Add (Val 3) (Val 4))

e3 :: Expr  -- Just 30
e3 = Add (Catch Throw (Val 10)) (Catch (Val 20) Throw)

e4 :: Expr  -- Nothing
e4 =  Catch (Add Throw (Val 1)) (Add (Val 1) Throw)
