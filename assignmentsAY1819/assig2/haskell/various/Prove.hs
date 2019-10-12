{-# LANGUAGE TemplateHaskell #-}
module Prove where
import Ex1
import Ex2

-- instance Monad ListBag where
--   return m lb         = lb
--   (f lb) (>>=) (g lb) =
-- -- main = do

newtype ZipList a = ZipList { getZipList :: [a] }
  deriving (Eq, Show)

instance Functor ZipList where
  fmap f (ZipList xs) = ZipList (map f xs)

instance Applicative ZipList where
  pure x                    = ZipList (repeat x)
  ZipList fs <*> ZipList xs = ZipList (zipWith (\f x -> f x) fs xs)

-- > getZipList $ ZipList [(+1),(*100),(*5)] <*> ZipList [1,2,3]
-- [2,200,15]


newtype CharList = CharList { getCharList :: [Char] } deriving (Eq, Show)

newtype Pair b a = Pair { getPair :: (a,b) }

instance Functor (Pair c) where
  fmap f (Pair (x,y)) = Pair (f x, y)

-- ==================================================================================
-- ==================================================================================
-- ==================================================================================
-- ==================================================================================

applyMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
applyMaybe Nothing  _  = Nothing
applyMaybe (Just x) f = f x


type Birds = Int
type Pole  = (Birds, Birds)

landLeft :: Birds -> Pole -> Maybe Pole
landLeft n (left,right)
     | abs ((left + n) - right) < 4 = Just (left + n, right)
     | otherwise                    = Nothing

landRight :: Birds -> Pole -> Maybe Pole
landRight n (left,right)
     | abs (left - (right + n)) < 4 = Just (left, right + n)
     | otherwise                    = Nothing

banana :: Pole -> Maybe Pole
banana _ = Nothing


x -: f = f x


-- \ > return (0,0) >>= landLeft 1 >>= landRight 4 >>= landLeft (-1) >>= landRight (-2)
--  > Nothing





    -- ghci> Just 3 >>= (\x -> Just (show x ++ "!"))
    -- Just "3!"

    -- ghci> Just 3 >>= (\x -> Just "!" >>= (\y -> Just (show x ++ y)))
    -- Just "3!"
-- In a do expression, every line is a monadic value. To inspect its result, we use <-. If we have a Maybe String and we bind it with <- to a variable, that variable will be a String, just like when we used >>= to feed monadic values to lambdas. The last monadic value in a do expression, like Just (show x ++ y) here, can't be used with <- to bind its result, because that wouldn't make sense if we translated the do expression back to a chain of >>= applications. Rather, its result is the result of the whole glued up monadic value, taking into account the possible failure of any of the previous ones.

foo :: Maybe String
foo = Just 3   >>= (\x ->
      Just "!" >>= (\y ->
      Just (show x ++ y)))

fooDo :: Maybe String
fooDo = do
  x <- Just 3
  y <- Just "!"
  return ((show x) ++ y)

routine :: Maybe Pole
routine =
  return (0,0)      >>= (\start  ->
  landLeft  2 start >>= (\first  ->
  landRight 2 first >>= (\second ->
  landLeft  1 second)))

routineDo :: Maybe Pole
routineDo = do
  start <- return (0,0)
  first <- landLeft 2 start
  second <- landRight 2 first
  landLeft 1 second

--  When we write a line in do notation without binding the monadic value with <-, it's just like putting >> after the monadic value whose result we want to ignore. We sequence the monadic value but we ignore its result because we don't care what it is and it's prettier than writing _ <- Nothing, which is equivalent to the above.

routineBanana :: Maybe Pole
routineBanana =
  return (0,0)      >>= (\start  ->
  landLeft  2 start >>= (\first  ->
  Nothing           >>
  landRight 2 first >>= (\second ->
  landLeft  1 second)))

routineBananaDo :: Maybe Pole
routineBananaDo = do
  start <- return (0,0)
  first <- landLeft 2 start
  Nothing --  same as _ <- Nothing
  second <- landRight 2 first
  landLeft 1 second

-- ghci> [1,2] >>= \n -> ['a','b'] >>= \ch -> return (n,ch)
-- [(1,'a'),(1,'b'),(2,'a'),(2,'b')]

listOfTuples = do
  n  <- [1,2,3]
  ch <- ['a','b','c']
  return (n,ch)
-- list comprehensions are just syntactic sugar for using lists as monads.
listOfTuplesLC = [(n,ch) | n <- [1,2,3], ch <- ['a','b','c'] ]




class Monad m => MonadPlus m where
  mzero :: m a
  mplus :: m a -> m a -> m a

instance MonadPlus [] where
  mzero = []
  mplus = (++)

guard :: (MonadPlus m) => Bool -> m ()
guard True  = return ()
guard False = mzero

-- filtering out non deterministic computations
-- ghci> [1..50] >>= (\x -> guard ('7' `elem` show x) >> return x)
-- [7,17,27,37,47]
type KnightPos = (Int,Int)

moveKnight :: KnightPos -> [KnightPos]
moveKnight (c, r) = do
  (c', r') <- [(c+2,r-1),(c+2,r+1),(c-2,r-1),(c-2,r+1)
              ,(c+1,r-2),(c+1,r+2),(c-1,r-2),(c-1,r+2)
              ]
  guard (c' `elem` [1..8] && r' `elem` [1..8])
  return (c',r')

in3 :: KnightPos -> [KnightPos]
in3 start = do
  first  <- moveKnight start
  second <- moveKnight first
  third  <- moveKnight second
  return third

in3' :: KnightPos -> [KnightPos]
in3' start = do
  first  <- moveKnight start
  second <- moveKnight first
  moveKnight second

-- in3 start = return start >>= moveKnight >>= moveKnight >>= moveKnight

canReachIn3 :: KnightPos -> KnightPos -> Bool
canReachIn3 start end = end `elem` in3 start
