-- author: Lapo Toloni, ID: 568235

{-# LANGUAGE TemplateHaskell #-}
module Ex5 where
import Ex1
import Ex2
import qualified Data.Char (chr)

-- 1
-- Prelude Monad typeclass definition
-- class Monad m where
--   return :: a -> m a
--   (>>=) :: m a -> (a -> m b) -> m b

--   (>>) :: m a -> m b -> m b
--   x >> y = x >>= \_ -> y
--   fail :: String -> m a
--   fail msg = error msg

-- instance Monad [] where
--  return x = [x]
--  here f ::  a -> [b]
--  xs >>= f = concat (map f xs)


-- Is equivalent to pure for Applicatives
-- return puts a value into the minimal context w.r.t.
-- the considered Monad/Computation kind/Container
returnLB :: (Eq a) => a -> ListBag a
returnLB a = singleton a

-- auxiliary function used for bindLB
-- this function folds a list of ListBag a into a ListBag a
-- using as mappend operation sumBag.
-- Example:
-- let test1 =              LB [(1,5),(2,2),(3,2)      ]
-- let test2 =              LB [(1,1),(2,2),      (4,3)]
-- unions [test1, test2] == LB [(1,6),(2,4),(3,2),(4,3)]
unions :: (Ord a) => [ListBag a] -> ListBag a
unions bagsList = foldr sumBagOrd empty bagsList

-- bind is like function application, only instead
-- of taking a normal value and feeding it to a normal function,
-- it takes a monadic value (that is, a value with a context)
-- and feeds it to a function that takes a normal value but returns a monadic value.
bindLB :: (Eq a, Ord b) => ListBag a -> (a -> ListBag b) -> ListBag b
bindLB bag fL = unions $ map fL $ toList bag
{- bindLB implementation
I took inspiration from the Monad [] implementation:
 . I worked on the list obtained via toList from of the passed ListBag
   since fL :: (a -> ListBag b)
   map fL listified :: [ListBag b]
The next step is to "concatenate" all the ListBags in the above mentioned
ListBag b by using the method unions, more precisely we take the union
since we are working with set-like data structures
-}

--2
{-
The following implementations is not accepted by the compiler
instance Monad ListBag where
  return v      = singleton v
  (>>=)  bag fL = unions $ map fL $ toList bag

we can't make ListBag an instance of Monad because:
 . singleton requires the constraint Eq a
 . (>>=)     requires the constraint Ord b if unions is implemented using sumBagOrd
                                     Eq  b if unions is implemented using SumBag

Moreover there's a hierarchy between typeclasses:
Functor > Applicative > Monad
and we know that ListBag can't be a Functor so It can't be a Monad.
Since GHC 7.10 this hierarchy is by default present in the language!
source : https://wiki.haskell.org/Functor-Applicative-Monad_Proposal#Using_Functor.2FApplicative_functions_in_monadic_code
-}


-- 3) Write some tests for the functions just implemented


--testLB1 = fromList [49, 50, 51, 49] :: ListBag Int
-- LB [(49,2),(50,1),(51,1)]
-- listifiedTest = toList testLB1 :: [Int]
-- encodeInt :: Int -> Maybe Char
-- encodeInt x
--   | x > maxBound    = Nothing
--   |otherwise        = Just (Data.Char.chr x)
        
-- fLTest = (\ x -> returnLB $ Data.Char.chr x) :: Int -> ListBag Char
-- fLTest 49 = LB [('1',1)]
-- fLTest2 = (\ x -> returnLB $ encodeInt x)



test1 = (returnLB $ (Just 1)) `bindLB` (\x -> returnLB ([x]) `bindLB` (\y -> returnLB $ replicate 5 y) )
-- LB [([[Just 1],[Just 1],[Just 1],[Just 1],[Just 1]],1)]
test2 = (returnLB $ (Just 1)) `bindLB` (\x -> returnLB ([x]) `bindLB` (\y -> returnLB $ y++y) )
-- LB [([Just 1,Just 1],1)]
test3 = (returnLB $ ([1,2,3])) `bindLB` (\x -> returnLB ([x]) `bindLB` (\y -> returnLB $ replicate 5 y) )
test4 = fromList [1,1,1,2,2,3,3,3,3,3] `bindLB` (\x -> returnLB [Just x,Just (-x)])