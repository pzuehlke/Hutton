------------------------------------------------------
--  EXERCISE 4.4 - PROGRAMMING IN HASKELL - HUTTON  --
------------------------------------------------------

(||) :: Bool -> Bool -> Bool
True || True    = True
True || False   = True
False || True   = True
False || False  = False

(||) :: Bool -> Bool -> Bool
(||) a b    | a == False && b == False  = False
            | otherwise                 = True            

(||) :: Bool -> Bool -> Bool
False || False  = False
_   || _        = True

(||) :: Bool -> Bool -> Bool
False || b  = b
True  || _  = True
