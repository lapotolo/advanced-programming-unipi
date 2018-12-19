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
mapLB :: (Eq b) => (a -> b) -> ListBag a -> ListBag b
mapLB _ (LB [])          = empty -- I cannot put empty on both sides because the compiler cannot infers if it's an empty of ListBag a or of ListBag b
mapLB f (LB ((x,k):xs))  = (singletonK (f x) k) ` sumBag `   ( mapLB f (LB xs) )


mapLB' :: (Ord b) => (a -> b) -> ListBag a -> ListBag b
mapLB' _ (LB [])          = empty
mapLB' f (LB ((x,k):xs))  = (singletonK (f x) k) ` sumBag' ` ( mapLB' f (LB xs) )

-- mapLB satisfies both the functor laws ie
--     mapLB id = id
--     mapLB (f . g) = fmap f . fmap g




{- Notes on Functor for ListBag

we can't make ListBag an instance of Functor because of the requirements to deal
with well formed ListBag and so because of the needed constraint (Eq a) on many functions in the module Ex1.
More precisely we cannot implement fmap respecting its original signature
fmap :: (a -> b) -> f a -> f b, since we should use a signature
fmap :: (Eq  b) => (a -> b) -> f a -> f b    (if we use sumBag  to concatenate recursive results), or
fmap :: (Ord b) => (a -> b) -> f a -> f b    (if we use sumBag')
To achieve this we cannot use the standard Functor class but we need something like:
http://hackage.haskell.org/package/constrained-categories-0.3.1.1/docs/Control-Functor-Constrained.html

!!! By relaxing the needness to work only with well-formed ListBag we could throw away the Eq constraint.


implementation trials:

instance Functor ListBag where
   fmap :: (Ord a, Ord b) => (a -> b) -> f a -> f b
   fmap _ (LB [])          = empty
   fmap f (LB ((x,k):xs))  = (singletonK (f x) k) `sumBag` ( fmap  f (LB xs) )

In a first moment I supposed that the impossibility of making ListBag an instance
of the functor typeclass was due to the introduction of the Ord constraint in
the function sumBag. So I gave another implementation of sumBag   without Ord constraints
but the error was still there because of the Eq constraint that is necessary to obtain
well formed ListBags
-}

-- trying to implement map using fold
-- mapLBFoldr :: (Ord b) => (a -> b) -> ListBag a -> ListBag b
-- mapLBFoldr f bag = foldr (\ x acc -> (f x) `sumBag` acc) empty bag

-- mapWithFoldr f xs = foldr'' ( \x acc -> (f x) : acc ) [] xs
-- foldr' f z []     = z
-- foldr' f z (x:xs) = x `f` foldr f z xs
