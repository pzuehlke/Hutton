-------------------------------------------------------
--  Exercise 16.9 - Programming in Haskell - Hutton  --
-------------------------------------------------------

data Maybe a = Just a | Nothing

instance Functor Maybe where
    -- fmap :: (a -> b) -> Maybe a -> Maybe b
    fmap _ Nothing  = Nothing
    fmap f (Just x) = Just (f x)

instance Applicative Maybe where
    -- pure :: a -> Maybe a
    pure = Just
    -- (<*>) :: Maybe (a -> b) -> Maybe a -> Maybe b
    Nothing <*> _       = Nothing
    (Just f) <*> mx     = fmap f mx

-- Theorem: The applicative laws hold for Maybe:
-- (i)     pure id <*> mx = mx
-- (ii)    pure (g x) = pure g <*> pure x
-- (iii)   mh <*> pure y = pure (\g -> g y) <*> mh
-- (iv)    mh <*> (mg <*> mx) = (pure (.) <*> mh <*> mg) <*> mx
--
-- Proof: In all cases, it is easier to figure out how to proceed by beginning
-- with the more complicated expression of the two.
--
-- (i)  We need to prove that
--          pure id <*> mx = mx.
--
--      pure id <*> mx      {applying pure}
--    = Just id <*> mx      {applying <*>}
--    = fmap id mx          {applying fmap, using an implicit case analysis}
--    = mx                  []
--
-- (ii) We need to prove that
--          pure (g x) = pure g <*> pure x.
--
--      pure g <*> pure x   {applying pure twice}
--    = Just g <*> Just x   {applying <*>}
--    = fmap g (Just x)     {applying fmap}
--    = Just (g x)          {unapplying pure}
--    = pure (g x)          [] 
--
-- (iii) We need to prove that
--          mh <*> pure y = pure (\g -> g y) <*> mh.
--       Notice that mh is of type Maybe (a -> b) (see exercise 12-5).
--
--       pure (\g -> g y) <*> mh    {applying pure}
--     = Just (\g -> g y) <*> mh    {applying <*>}
--     = fmap (\g -> g y) mh        {... to be continued}
--       Now we need to consider the two cases for mh separately:
--           * If mh = Nothing, then continuing we get:
--             = fmap (\g -> g y) Nothing   {applying fmap}
--             = Nothing                    {unapplying <*>}
--             = Nothing <*> pure y         {hypothesis: mh = Nothing}
--             = mh <*> pure y              []
--           * If mh = Just h, then continuing we get:
--             = fmap (\g -> g y) (Just h)  {applying fmap}
--             = Just ((\g -> g y) h)       {applying the lambda}
--             = Just (h y)                 {unappyling fmap}
--             = fmap h (Just y)            {unapplying <*>}
--             = (Just h) <*> (Just y)      {unapplying pure}
--             = (Just h) <*> pure y        {hypothesis: mh = Just h}
--             = mh <*> pure y              []
--
-- (iv) We need to prove that
--          mh <*> (mg <*> mx) = (pure (.) <*> mh <*> mg) <*> mx
--      Here (cf. exercise 12-5):
--      * mh :: Maybe (b -> c)
--      * mg :: Maybe (a -> b)
--      * mx :: Maybe a
--
--      It would be tedious to do a case-by-case analysis since there are 8
--      possibilities to consider, so we can instead argue as follows. Notice
--      from the definition of <*> (and of fmap) that if either of the operands
--      is Nothing, then the result of an <*> operation will also be Nothing.
--      It follows that any of mh, mg or mx is Nothing, this Nothing will
--      propagate and both sides of the equation that we have to establish will
--      evaluate to Nothing.
--
--      On the other hand, if mh, mg and mx are all of the form Just _, then
--      it also follows from the definitions that <*> is simply the usual
--      function application on the naked values, with `Just` tagged at the
--      end. Therefore in this case the left side equals Just (h (g x)), while
--      the right side equals Just ((h . g) x), which clearly coincide.     []
