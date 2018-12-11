{-# LANGUAGE TemplateHaskell #-}
module Ex2 where

import Ex1

-- Just to remember how folds are implemented on lists:
-- foldr' f z []     = z
-- foldr' f z (x:xs) = x `f` foldr f z xs
--
-- foldl' f z []     = z
-- foldl' f z (x:xs) = let z' = z `f` x
--                     in seq z' $ foldl' f z' xs
--


-- since the exercise tells to fold only over the elements of the ListBag we can just implement the foldr
-- following the schema used in the prelude to implement the foldr for lists. We only need to care to
-- pattern match using the LB constructor provided for the ListBag type
instance Foldable ListBag where
--  foldr :: (a -> b -> b) -> b -> ListBag a -> b  -- signature not required when instantiating a typeclass
  foldr f z (LB [])     = z
  foldr f z (LB (x:xs)) = (fst x) `f` foldr f z (LB xs)


-- since we are dealing with a ListBag we cannot concatenate and recur using (:) but we can exploit the sumBag function
-- to concatenate singleton by singleton what we obtained after having applied the f function passed to the map
mapLB :: (Ord b) => (a -> b) -> ListBag a -> ListBag b
mapLB _ (LB [])          = empty -- I cannot put empty on both sides because the compiler cannot infers if it's an empty of ListBag a or of ListBag b
mapLB f (LB ((x,k):xs))  = (singletonK (f x) k) `sumBag` ( mapLB f (LB xs) )

-- trying to implement map using fold
-- mapLBFoldr :: (Ord b) => (a -> b) -> ListBag a -> ListBag b
-- mapLBFoldr f bag = foldr (\ x acc -> (f x) `sumBag` acc) empty bag

-- notes
-- mapWithFoldr f xs = foldr'' ( \x acc -> (f x) : acc ) [] xs
-- foldr' f z []     = z
-- foldr' f z (x:xs) = x `f` foldr f z xs

-- TODO
-- explain why we cant make ListBag an instance of Functor
