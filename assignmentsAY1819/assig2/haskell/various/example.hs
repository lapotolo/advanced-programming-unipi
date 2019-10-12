{-# LANGUAGE TemplateHaskell #-}
module Ex1 where

import Data.List

data ListBag a = LB [(a, Int)] deriving (Show, Eq)
data Person = Person
  { name :: String
  , age  :: Int
  , gender :: Either String String
  , father :: Maybe Person
  , mother :: Maybe Person
  }

-- father :: Person -> Maybe Person
-- mother :: Person -> Maybe Person

maternalGrandfather :: Person -> Maybe Person
maternalGrandfather p =
    case mother p of
        Nothing -> Nothing
        Just mom -> father mom


maternalGrandfather' :: Person -> Maybe Person
maternalGrandfather' p = mother p >>= father

paternalGrandfather' :: Person -> Maybe Person
paternalGrandfather' p = father p >>= father

maternalGrandmother' :: Person -> Maybe Person
maternalGrandmother' p = mother p >>= mother

paternalGrandmother' :: Person -> Maybe Person
paternalGrandmother' p = father p >>= mother

bothGrandfathers :: Person -> Maybe (Person, Person)
bothGrandfathers p =
  case father p of
    Nothing -> Nothing
    Just dad ->
        case father dad of
          Nothing -> Nothing
          Just gf1 ->           -- found first grandfather
            case mother p of
              Nothing -> Nothing
              Just mom ->
                case father mom of
                   Nothing -> Nothing
                   Just gf2 ->  -- found second grandfather
                     Just (gf1, gf2)

-- To be a little more precise: The result of father p is a monadic value
-- (in this case, either Just dad or Nothing, depending on whether p's father is in the database).
-- As the father function takes a regular (non-monadic value), the (>>=) feeds p's dad to it as a non-monadic value.
-- The result of father dad is then monadic again, and the process continues.

-- So, (>>=) helps us pass non-monadic values to functions without actually leaving a monad.
-- In the case of the Maybe monad, the monadic aspect is the qualifier that we don't know with certainty whether the value will be found.

bothGrandfathers' :: Person -> Maybe (Person, Person)
bothGrandfathers' p =
    father p >>= (\dad -> father dad >>= (\gf1 -> mother p >>= (\mom -> father mom >>= (\gf2 -> return (gf1,gf2) ))))

bothGrandfathers'' p = do {
    dad <- father p;
    gf1 <- father dad;
    mom <- mother p;
    gf2 <- father mom;
    return (gf1, gf2);
  }

-- class Applicative m => Monad m where
--   return :: a -> m a
--   (>>=)  :: m a -> (a -> m b) -> m b -- bind === semicolon and assigment

--   (>>)   :: m a -> m b -> m b -- then === semicolon       m >> n = m >>= \_ -> n
--   fail   :: String -> m a


-- (*>) :: Applicative f => f a -> f b -> f b
-- (>>) :: Monad m => m a -> m b -> m b

-- pure :: Applicative f => a -> f a
-- return :: Monad m => a -> m a
