{-# LANGUAGE TemplateHaskell #-}
module Ex1 where

import Data.List
data ListBag a = LB [(a, Int)] deriving (Show, Eq)


getListBagElements :: ListBag a -> [a]
getListBagElements (LB bag) = fst $ unzip bag


-- A ListBag is well-formed if it does not contain two pairs
--(v, k) and  (v', k') with v = v'.
wf :: (Eq a) => ListBag a -> Bool
wf (LB bag) = (==) (length onlyElements) (length $ nub onlyElements)
           where onlyElements = getListBagElements $ LB bag

-- getListBagElements (LB((x,y):xs)) = x : getListBagElements (LB xs)
-- getListBagElements bag = case bag of LB bag -> fst(head bag) : getListBagElements (LB(tail(bag)))

-- mapListBagElements :: (a -> b) -> ListBag a -> [b]
-- mapListBagMult :: ListBag a ->

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

groupMerged :: (Eq a, Ord a) => [a] -> [[a]]
groupMerged xs = group' $ sort xs

fromList :: (Eq a, Ord a) => [a] -> ListBag a
fromList [] = empty
fromList xs =  LB $ zip (nub $ sort xs) (map length (groupMerged xs))


count :: Eq a => a -> [a] -> Int
count x = length . filter (x ==)

isEmpty :: (Eq a) => ListBag a -> Bool
isEmpty bag
   | bag == empty = True
   | otherwise    = False



mul :: (Eq a) => a -> ListBag a -> Int
mul v bag = length $ filter (v ==) (toList bag)

-- not efficient at all!
sumBag :: (Eq a, Ord a) => ListBag a -> ListBag a -> ListBag a
sumBag bag1 bag2 = fromList(list1 ++ list2)
          where list1 = toList bag1
                list2 = toList bag2

-- unique :: [Int] -> [Int]
-- unique xs = [x | (x,y) <- zip xs [0..], x `notElem` (take y xs)]

-- insertBagList :: (Eq a) => ListBag a -> a -> ListBag a
-- insertBagList empty v         = (singleton v)
-- insertBagList ((x,k):xs) v
--            | (==) x v  = LB([(x, k+1)]) : LB(xs)
--            | otherwise = insertBagList xs v

-- wf :: (Eq a) => ListBag a -> Bool
-- wf empty = True
-- wf ((x,k):xs) = (&&)  (wf xs)

