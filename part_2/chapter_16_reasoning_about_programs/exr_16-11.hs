-------------------------------------------------------
--  Exercise 16.11 - Programming in Haskell - Hutton  --
-------------------------------------------------------

data Expr = Val Int | Add Expr Expr
data Op = PUSH Int | ADD deriving Show
type Code = [Op]

comp :: Expr -> Code
comp (Val n)    = [PUSH n]
comp (Add x y)  = comp x ++ comp y ++ [ADD]

-- We need to figure out how to define comp' without using comp in the
-- definition, subject to the requirement:
comp' e c = comp e ++ c

-- We can guess the definition and prove that this requirement uniquely
-- specifies comp' using induction on the complexity/depth of the expression e:

-- Base case: e = Val n
-- = comp' (Val n) c    {using the condition}
-- = comp (Val n) ++ c  {applying comp}
-- = [PUSH n] ++ c      {applying ++}
-- = (PUSH n) : c
--
-- Inductive case: e = Add x y
-- = comp' (Add x y) c                  {using the condition}
-- = comp (Add x y) ++ c                {applying comp}
-- = (comp x ++ comp y ++ [ADD]) ++ c   {associativity of ++}
-- = comp x ++ (comp y ++ ([ADD] ++ c)) {applying rightmost ++}
-- = comp x ++ (comp y ++ (ADD : c))    {induction hypothesis}
-- = comp x ++ (comp' y (ADD:c))        {induction hypothesis again}
-- = comp' x (comp' y (ADD:c))          []
