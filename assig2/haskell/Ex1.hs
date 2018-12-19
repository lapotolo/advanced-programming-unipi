{-# LANGUAGE TemplateHaskell #-}
module Ex1 where

import Data.List

data ListBag a = LB [(a, Int)] deriving (Show, Eq)


getListBagElements :: ListBag a -> [a]
getListBagElements (LB bag) = fmap fst bag

getListBagMultuplicities :: ListBag a -> [Int]
getListBagMultuplicities (LB bag) = fmap snd bag

-- IMPLEMENTATION CHOICE
-- I preferred to put in almost every function the constraint (Eq a)
-- since to test for equality is needed to test for well formedness

-- A ListBag is well-formed if it does not contain two pairs
--(v, k) and  (v', k') with v = v'.
wf :: (Eq a) => ListBag a -> Bool
wf (LB bag) = (==) (length onlyElements) (length $ nub onlyElements)
           where onlyElements = getListBagElements $ LB bag

empty :: ListBag a
empty = LB []

singleton :: (Eq a) => a -> ListBag a
singleton v = LB [(v, 1)]

singletonK :: (Eq a) => a -> Int -> ListBag a
singletonK v k = LB [(v, k)]

toListAux :: (Eq a) => [(a,Int)] -> [a]
toListAux [] = []
toListAux ((x,k):xs) = replicate k x ++ toListAux xs

toList :: (Eq a) => ListBag a -> [a]
toList bag = case bag of LB bag -> toListAux bag

-- group' [1,1,2,3,4,5,5,1,1] = [[1,1],[2],[3],[4],[5,5],[1,1]]
group' :: (Eq a) => [a] -> [[a]]
group' []     = []
group' (x:xs) = (x : takeWhile (== x) xs) : group' (dropWhile (== x) xs)

-- composing the group' function with a sort we get a list for each different element of the original list
groupMerge :: (Eq a, Ord a) => [a] -> [[a]]
groupMerge xs = group' $ sort xs

fromList :: (Eq a, Ord a) => [a] -> ListBag a
fromList [] = empty
fromList xs =  LB $ zip (nub $ sort xs) (map length (groupMerge xs))

count :: Eq a => a -> [a] -> Int
count x = length . filter (x ==)

isEmpty :: (Eq a) => ListBag a -> Bool
isEmpty bag
   | bag == empty = True
   | otherwise    = False

mul :: (Eq a) => a -> ListBag a -> Int
mul v bag = length $ filter (v ==) (toList bag)


{-  =============================================================================

In the following lines of code I gave two different implementations of sumBag
both for efficiency reason since sumBag' always produces sorted by elements
ListBag and for experimenting with typeclasses.
 . sumBag  need only the type constraint Eq a
 . sumBag' need 
 ============================================================================= -}


-- get the elements of two ListBag no duplicates
getElements :: (Eq a) => ListBag a -> ListBag a -> [a]
getElements bag1 bag2 = nub $ (getListBagElements bag1) ++ (getListBagElements bag2)

-- lookup :: Eq a => a -> [(a, b)] -> Maybe b
-- Defined in GHC.List
-- I m interpreting the value of a ListBag as a key and its multiplicity as its mapped
-- value. Just like in a dictionary
-- getMult 3 (LB [(1,3),(3,2),(7,1)]) is 2 ei "value 3 is in that LB with mult 2"
getMult :: (Eq a) => a -> ListBag a -> Int
getMult element (LB xs) = case lookup element xs of
  Nothing -> 0 -- x is not in the list
  Just n  -> n -- x is in the list associated with n

-- two ListBag are equals iff for every value in the union
-- of the keys of the two every key maps to the same
-- multiplicity
equalListBags :: (Eq a) => ListBag a -> ListBag a -> Bool
equalListBags bag1 bag2  = all test (getElements bag1 bag2)
  where
    -- Check that a key maps to the same value in both ListBag
    test element =  (==) (getMult element bag1) (getMult element bag2)

-- finish here please!
sumBag :: (Eq a) => ListBag a -> ListBag a -> ListBag a
sumBag bag1 bag2 = LB $ map sumMultSameElement (getElements bag1 bag2)
   where
     -- Build a new list element from a key
     sumMultSameElement ele = (ele, ( (+) (getMult ele bag1) (getMult ele bag2) ))



-- Note that I introduced the constraint Ord a to get some performance improvements
-- costs assuming: n1    = #elements in bag1
--                 n2    = #elements in bag1
--                 nMax  = max n1 n2
--                 k1Max = max multiplicity in bag1
--                 k2Max = max multiplicity in bag2
--                 kMax  = max k1Max k2Max
-- TIME: toList   costs O(n1 * k1Max) + O(n2 * k2Max) = O(nMax * kMax)
--       fromList costs O(nMax * log Nmax)
--       total time is  O(n*log n)
sumBag' :: (Eq a, Ord a) => ListBag a -> ListBag a -> ListBag a
sumBag' bag1 bag2 = fromList(list1 ++ list2)
          where list1 = toList bag1
                list2 = toList bag2
