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
-- I followed the schema used in the prelude to implement the foldr. We only need to care to
-- pattern match using the LB constructor provided for the ListBag type
instance Foldable ListBag where
-- foldr :: (a -> b -> b) -> b -> ListBag a -> b  -- signature not required when instantiating a typeclass
  foldr _ z (LB [])     = z
  foldr f z (LB (x:xs)) = (fst x) `f` foldr f z (LB xs)

-- since we are dealing with a ListBag we cannot concatenate and recur using (:) but we can exploit the sumBag function
-- to concatenate singleton by singleton what we obtained after having applied the f function passed to the map
mapLB :: (Ord b) => (a -> b) -> ListBag a -> ListBag b
mapLB _ (LB [])          = empty -- I cannot put empty on both sides because the compiler cannot infers if it's an empty of ListBag a or of ListBag b
mapLB f (LB ((x,k):xs))  = (singletonK (f x) k) `sumBag` ( mapLB f (LB xs) )

-- mapLB satisfies both the functor laws ie
--     mapLB id = id
--     mapLB (f . g) = fmap f . fmap g


{- Notes of Functor for ListBag

we can't make ListBag an instance of Functor because of the requirements to deal
with well formed ListBag and the needed constraint Eq a
More precisely we cannot implement fmap respecting its original signature
fmap :: (a -> b) -> f a -> f b, since we should use a signature
fmap :: (Eq a, Eq b) => (a -> b) -> f a -> f b
ie, we need more constraints on a and b, Eq and Ord.
To achieve this we cannot use the standard Functor class but we need something like


implementation trials:

instance Functor ListBag where
   --fmap :: (Ord a, Ord b) => (a -> b) -> f a -> f b
   fmap _ (LB [])          = empty
   fmap f (LB ((x,k):xs))  = (singletonK (f x) k) `sumBag` ( fmap  f (LB xs) )

In a first moment I supposed that the impossibility of making ListBag an instance
of the functor typeclass was due to the introduction of the Ord constraint in
the function sumBag. So I gave another implementation of sumBag  without Ord constraints
but the error was still there because of the Eq constraint that is necessary to obtain
well formed ListBags
-}

-- trying to implement map using fold
-- mapLBFoldr :: (Ord b) => (a -> b) -> ListBag a -> ListBag b
-- mapLBFoldr f bag = foldr (\ x acc -> (f x) `sumBag` acc) empty bag

-- mapWithFoldr f xs = foldr'' ( \x acc -> (f x) : acc ) [] xs
-- foldr' f z []     = z
-- foldr' f z (x:xs) = x `f` foldr f z xs
