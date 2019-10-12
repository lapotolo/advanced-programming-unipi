-- author: Lapo Toloni, ID: 568235


{-# LANGUAGE TemplateHaskell #-}
module Ex1 where

import Data.List

data ListBag a = LB [(a, Int)] deriving (Show, Eq)

--
-- nub l                   = nub' l []             -- '
--   where
--     nub' [] _           = []                    -- '
--     nub' (x:xs) ls                              -- '
--         | x `elem` ls   = nub' xs ls            -- '
--         | otherwise     = x : nub' xs (x:ls)    -- '


-- elem, notElem    :: (Eq a) => a -> [a] -> Bool
-- elem x           =  any (== x)
-- notElem x        =  all (/= x)

-- ===============================================
elements :: ListBag a -> [a]
elements (LB bag) = fmap fst bag

multiplicities :: ListBag a -> [Int]
multiplicities (LB bag) = fmap snd bag

-- IMPLEMENTATION CHOICE
-- I preferred to put in almost every function the constraint (Eq a)
-- since to test for equality is needed to test for well formedness

-- A ListBag is well-formed if it does not contain two pairs
--(v, k) and  (v', k') with v = v'.

wf :: (Eq a) => ListBag a -> Bool
wf (LB bag) = (==) (length onlyElements) (length $ nub onlyElements)
           where onlyElements = elements $ LB bag

empty :: ListBag a
empty = LB []

singleton :: (Eq a) => a -> ListBag a
singleton v = LB [(v, 1)]

singletonK :: (Eq a) => a -> Int -> ListBag a
singletonK v k = LB [(v, k)]

-- deprecated. It seems to be more ideomatic
-- to use a toList' defined in the where clause
-- of the function toList
--
-- toListAux :: (Eq a) => [(a,Int)] -> [a]
-- toListAux [] = []
-- toListAux ((x,k):xs) = replicate k x ++ toListAux xs

toList :: (Eq a) => ListBag a -> [a]
toList (LB bag) = toList' bag
  where toList' []         = []
        toList' ((x,k):xs) = replicate k x ++ toList' xs

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

-- count the number of times the element e appears in the
-- ListBag. Multiplicities are ignored here
-- auxiliary that can  be used to implement wf
count :: (Eq a) => a -> ListBag a -> Int
count e = length . filter (e ==) . elements

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
 . sumBag  need the type constraint Eq a
 . sumBag' need the type constraint Ord a
 ============================================================================= -}


-- get the elements of two ListBag no duplicates
getElements :: (Eq a) => ListBag a -> ListBag a -> [a]
getElements bag1 bag2 = nub $ (elements bag1) ++ (elements bag2)

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

-- sumBag use examples:
-- sumBag (LB[(1,2),(2,2),(4,2)]) (LB[(3,3)])       == LB [(1,2),(2,2),(4,2),(3,3)]
-- sumBag (LB[(1,2),(2,2),(4,2)]) (LB[(2,3),(3,1)]) == LB [(1,2),(2,5),(4,2),(3,1)]
sumBag :: (Eq a) => ListBag a -> ListBag a -> ListBag a
sumBag bag1 bag2 = LB $ map sumMultSameElement (getElements bag1 bag2)
   where
     -- Build a new list element from a key
     sumMultSameElement ele = (ele, ( (+) (getMult ele bag1) (getMult ele bag2) ))



-- alternative implementation of sumBag that always returns a ListBag sorted by elements
-- assuming:  n1    = #elements in bag1
--            n2    = #elements in bag1
--            nMax  = max n1 n2
--            k1Max = max multiplicity in bag1
--            k2Max = max multiplicity in bag2
--            kMax  = max k1Max k2Max
-- TIME COST: toList   costs O(n1 * k1Max) + O(n2 * k2Max) = O(nMax * kMax)
--            fromList costs O(nMax * log Nmax)
--            total time is  O(n*log n)
sumBagOrd :: (Eq a, Ord a) => ListBag a -> ListBag a -> ListBag a
sumBagOrd bag1 bag2 = fromList(list1 ++ list2)
          where list1 = toList bag1
                list2 = toList bag2
