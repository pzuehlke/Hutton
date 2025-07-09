------------------------------------------------------
--  EXERCISE 4.4 - PROGRAMMING IN HASKELL - HUTTON  --
------------------------------------------------------

or :: Bool -> Bool -> Bool
or True  True   = True
or True  False  = True
or False True   = True
or False False  = False

or' :: Bool -> Bool -> Bool
or' a b | a == False && b == False  = False
        | otherwise                 = True            

or'' :: Bool -> Bool -> Bool
or'' False False  = False
or'' _     _      = True

or''' :: Bool -> Bool -> Bool
or''' False b  = b
or''' True  _  = True
