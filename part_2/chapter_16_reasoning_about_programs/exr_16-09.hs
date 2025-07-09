-------------------------------------------------------
--  Exercise 16.9 - Programming in Haskell - Hutton  --
-------------------------------------------------------

data Maybe a = Just a | Nothing

instance Functor Maybe where
  fmap _ Nothing  = Nothing
  fmap f (Just x) = Just (f x)

instance Applicative Maybe where
  pure = Just
  Nothing <*> _       = Nothing
  (Just f) <*> mx     = fmap f mx

-- Theorem: Applicative laws for Maybe:
--          (i)   pure id <*> v = v                    (identity)
--          (ii)  pure (.) <*> u <*> v <*> w = u <*> (v <*> w)  (composition)
--          (iii) pure f <*> pure x = pure (f x)      (homomorphism)
--          (iv)  u <*> pure y = pure ($ y) <*> u     (interchange)
--
-- Proof:
-- (i) Identity law: By case analysis on v:
--      pure id <*> Nothing      {applying pure, <*>}
--    = Just id <*> Nothing      {applying <*>}
--    = Nothing
--
--      pure id <*> (Just x)     {applying pure, <*>}
--    = Just id <*> (Just x)     {applying <*>}
--    = fmap id (Just x)         {applying fmap}
--    = Just (id x)              {applying id}
--    = Just x
--
-- (ii) Composition law: By case analysis on u:
--      pure (.) <*> Nothing <*> v <*> w    {applying pure, <*>}
--    = Just (.) <*> Nothing <*> v <*> w    {applying <*>}
--    = Nothing <*> v <*> w                 {applying <*>}
--    = Nothing <*> w                       {applying <*>}
--    = Nothing
--
--      Nothing <*> (v <*> w)    {applying <*>}
--    = Nothing
--
--      Case u = Just f: By case analysis on v:
--      pure (.) <*> (Just f) <*> Nothing <*> w    {applying pure, <*>}
--    = Just (.) <*> (Just f) <*> Nothing <*> w    {applying <*>}
--    = fmap (.) (Just f) <*> Nothing <*> w        {applying fmap}
--    = Just ((.) f) <*> Nothing <*> w             {applying <*>}
--    = Nothing <*> w                              {applying <*>}
--    = Nothing
--
--      (Just f) <*> (Nothing <*> w)    {applying <*>}
--    = (Just f) <*> Nothing           {applying <*>}
--    = Nothing
--
--      Case u = Just f, v = Just g: By case analysis on w:
--      pure (.) <*> (Just f) <*> (Just g) <*> Nothing    {applying pure, <*>}
--    = Just (.) <*> (Just f) <*> (Just g) <*> Nothing    {applying <*>}
--    = fmap (.) (Just f) <*> (Just g) <*> Nothing        {applying fmap}
--    = Just ((.) f) <*> (Just g) <*> Nothing             {applying <*>}
--    = fmap ((.) f) (Just g) <*> Nothing                 {applying fmap}
--    = Just (((.) f) g) <*> Nothing                      {applying <*>}
--    = Nothing
--
--      (Just f) <*> ((Just g) <*> Nothing)    {applying <*>}
--    = (Just f) <*> Nothing                  {applying <*>}
--    = Nothing
--
--      pure (.) <*> (Just f) <*> (Just g) <*> (Just x)    {applying pure, <*>}
--    = Just (.) <*> (Just f) <*> (Just g) <*> (Just x)    {applying <*>}
--    = fmap (.) (Just f) <*> (Just g) <*> (Just x)        {applying fmap}
--    = Just ((.) f) <*> (Just g) <*> (Just x)             {applying <*>}
--    = fmap ((.) f) (Just g) <*> (Just x)                 {applying fmap}
--    = Just (((.) f) g) <*> (Just x)                      {applying <*>}
--    = fmap (((.) f) g) (Just x)                          {applying fmap}
--    = Just ((((.) f) g) x)                               {applying (.)}
--    = Just (f (g x))
--
--      (Just f) <*> ((Just g) <*> (Just x))    {applying <*>}
--    = (Just f) <*> (fmap g (Just x))         {applying fmap}
--    = (Just f) <*> (Just (g x))              {applying <*>}
--    = fmap f (Just (g x))                    {applying fmap}
--    = Just (f (g x))                         []
--
-- (iii) Homomorphism law:
--      pure f <*> pure x        {applying pure}
--    = Just f <*> Just x        {applying <*>}
--    = fmap f (Just x)          {applying fmap}
--    = Just (f x)               {unapplying pure}
--    = pure (f x)               []
--
-- (iv) Interchange law: By case analysis on u:
--      Nothing <*> pure y       {applying pure}
--    = Nothing <*> Just y       {applying <*>}
--    = Nothing
--
--      pure ($ y) <*> Nothing   {applying pure, <*>}
--    = Just ($ y) <*> Nothing   {applying <*>}
--    = Nothing
--
--      (Just f) <*> pure y      {applying pure}
--    = (Just f) <*> Just y      {applying <*>}
--    = fmap f (Just y)          {applying fmap}
--    = Just (f y)
--
--      pure ($ y) <*> (Just f)  {applying pure, <*>}
--    = Just ($ y) <*> (Just f)  {applying <*>}
--    = fmap ($ y) (Just f)      {applying fmap}
--    = Just (($ y) f)           {applying $}
--    = Just (f y)               []
