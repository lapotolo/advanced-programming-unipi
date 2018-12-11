{-# LANGUAGE TemplateHaskell #-}
module Ex2 where

import Ex1

--foldr' f z []     = z
--foldr' f z (x:xs) = x `f` foldr f z xs

-- foldl' f z []     = z
-- foldl' f z (x:xs) = let z' = z `f` x
--                     in seq z' $ foldl' f z' xs

instance Foldable ListBag where
--  foldr :: (a -> b -> b) -> b -> ListBag a -> b
  foldr f z (LB [])     = z
  foldr f z (LB (x:xs)) = (fst x) `f` foldr f z (LB xs)

  -- foldr f z (LB (x:xs)) = (head onlyElements) `f` foldr f z (LB(tail bag))
  --                    where onlyElements = fst $ unzip bag


-- scanr f z empty    = z
-- scanr f z (LB bag) = (head onlyElements) `f` scanr f z (LB(tail bag))
--                      onlyElements = fst $ unzip bag


mapLB :: (Ord b) => (a -> b) -> ListBag a -> ListBag b
mapLB f (LB [])          = empty -- I cant put empty on both sides because the compiler cannot infers if it's an empty of ListBag a or of ListBag b
mapLB f (LB ((x,k):xs))  = (singletonK (f x) k) `sumBag` ( mapLB f (LB xs) )

