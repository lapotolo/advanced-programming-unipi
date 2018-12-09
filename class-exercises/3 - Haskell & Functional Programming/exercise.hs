import Data.Char

-- EXERCISE 1
-- Recursive implementation
myReplicateRec :: Int -> Int -> [Int]
myReplicateRec 0 _ = []
myReplicateRec n v = v : myReplicateRec (n-1) v


-- Combinator implementation map filter foldl/r
myReplicateCom :: Int -> Int -> [Int]
myReplicateCom n v = map (const v) [1..n]

-- myFoldr :: ( a -> a ) -> [a] -> a
myFoldr f z []     = z
myFoldr f z (x:xs) = f (x) (myFoldr f z xs)
--myFoldr f z (x:xs) = x `f` (myFoldr f z xs)


myFoldl f z []     = z
myFoldl f z (x:xs) = let z' = z `f` x
                     in myFoldl f z' xs

myFoldlSeq f z []     = z
myFoldlSeq f z (x:xs) = let z' = z `f` x
                        in seq z' $ myFoldlSeq f z' xs

--myReplicateFold :: Integer -> Integer -> [Integer]
--myReplicateFold n v = foldl' ((:) . const) v []

-- doubleChar "String" -- returns "SSttrriinngg"
doubleChar :: String -> String
doubleChar s = foldr (\a b -> a:a:b) [] s
-- map' f xs = foldr (\x acc -> f x : acc) [] xs

-- EXERCISE 2
sumOddRec :: [Int] -> Int
sumOddRec [] = 0
sumOddRec (x:xs)
  | (/=) (mod x 2) 0 =  x + (sumOddRec xs)
  | otherwise        =  0 + (sumOddRec xs)

sumOddCom :: [Int] -> Int
sumOddCom xs = foldr (\ acc x -> acc + x) 0 (filter odd xs)

-- EXERCISE 3
replElemsRec :: [Int] -> Int -> [Int]
replElemsRec [] n      = []
replElemsRec (x:xs) n = (myReplicateRec n x) ++ (replElemsRec xs n)

replElemsCom :: [Int] -> Int -> [Int]
replElemsCom xs n = concat (map (myReplicateCom n) xs)

-- WRONG
-- concat' :: [[a]] -> [a]
-- concat' [] = []
-- concat' [[xs]] = [xs]
-- concat' ((x:xs):xxs) = x : concat' (xs:xxs)

-- EXERCISE 4
totalLengthCom :: [String] -> Int
totalLengthCom xs = foldr (+) 0 (map length(filter (\ word -> if (head word) == 'A' then True else False) xs))

-- EXERCISE 5
-- broken
-- filterOddRec :: [a] -> [a]
-- filterOddRec [] = []
-- filterOddRec xs
--   | (odd . snd) (head (zip xs [1..])) = (head xs) : (filterOddRec xs)
--   | otherwise                       = filterOddRec (tail xs)

filterOddRecAux :: [a] -> Int -> [a]
filterOddRecAux [] index = []
filterOddRecAux (x:xs) index
  | odd index  = x : filterOddRecAux xs (succ index)
  | otherwise  =     filterOddRecAux xs (succ index)

filterOddRec :: [a] -> [a]
filterOddRec xs = filterOddRecAux xs 1

filterOddCom :: [a] -> [a]
filterOddCom xs = map fst (filter (odd . snd) (zip xs [1..]))

-- EXERCISE 6

--titlecase :: String -> String
--titlecase s@(x:xs) = unwords (map ((toUpper x) : xs) (words s))

-- EXERCISE 7
-- countVowelPali :: [String] -> Int
-- countVowelPali xs = foldr (+) 0 (map length (filter (isPali ) xs))
--   where isPali     word = (==) (word) (reverse word)
--         onlyVowels word = (elem "aeiouyAEIOUY") word

-- EXERCISE 8
-- foldl'' :: (Foldable t) => (b -> a -> b) -> b -> t a -> b
foldl'' f z []     = z
foldl'' f z (x:xs) = foldl'' f (z `f` x) xs

-- miss seq foldl''

-- foldr'' :: (Foldable t) => (a -> b -> b) -> b -> t a -> b
foldr'' f z []     = z
foldr'' f z (x:xs) = x `f` (foldr'' f z xs)

mapWithFoldlRev :: (a -> b) -> [a] -> [b] -- worse performance
mapWithFoldlRev f xs = reverse (foldl'' ( \acc x -> (f x) : acc ) [] xs)

mapWithFoldl :: (a -> b) -> [a] -> [b] -- using concat
mapWithFoldl f xs = foldl'' ( \acc x -> acc ++ [(f x)] ) [] xs

mapWithFoldr :: (a -> b) -> [a] -> [b]
mapWithFoldr f xs = foldr'' ( \x acc -> (f x) : acc ) [] xs

-- EXERCISE 9 have fun with binary trees
data IntTree = Leaf Int | Node (Int, IntTree, IntTree) deriving (Show)

-- 1
testTree1 = Leaf 5
testTree2 = Node (5, Node(6, Leaf 7,  Leaf 8 ),
                     Node(9, Leaf 10, Leaf 11))

tmap :: (Int -> Int) -> IntTree -> IntTree
tmap f (Leaf x) = Leaf (f x)
tmap f (Node (x, child1, child2)) = Node (f x , tmap f child1, tmap f child2)

-- 2
succTree :: IntTree -> IntTree
succTree t = tmap succ t

--3
--foldrIntTree :: (Int -> Int) -> Int -> IntTree -> Int
foldrIntTree f z (Leaf x) = x
foldrIntTree f z (Node(x, child1, child2)) = x `f` (foldrIntTree f z child1)
                                               `f` (foldrIntTree f z child2)


sumSucc :: IntTree -> Int
sumSucc t = foldrIntTree (+) 0 (tmap succ t)

-- EXERCISE 10
-- Implement a tail recursive version of the map and filter combinators
map' :: (a -> b) -> [a] -> [b]
map' f []     = []
map' f (x:xs) = (f x) : map' f xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ []  = []
filter' p (x:xs)
  | p x       = x : filter' p xs
  | otherwise =     filter' p xs

mapTailRec :: (a -> b) -> [a] -> [b]
mapTailRec f xs = mapAux (f, xs, [])
  where mapAux (f, [],   accum) = accum
        mapAux (f, y:ys, accum) = mapAux (f, ys, accum ++ [(f y)])

-- mapTailRec f xs = mapAux (f, xs, [])
--   where mapAux (f, [],   accum) = reverse accum
--         mapAux (f, y:ys, accum) = mapAux (f, ys, (f y):ys)

-- filterTailRec :: (a -> Bool) -> [a] -> [a]
-- filterTailRec p xs = filterAux (p, xs, [])
--   where filterAux (p, [],   accum) = reverse accum
--         filterAux (p, y:ys, accum) = filterAux (if p y then y:accum else accum, ys, accum ++ [])

-- trFilterAux acc [] f = reverse acc
-- trFilterAux acc (h:t) f = trFilterAux (if f h then h : acc else acc) t f

-- trFilter f x = trFilterAux [] x f

-- Bonus
-- quadratic, no tail recursion
reverseRec :: [a] -> [a]
reverseRec [] = []
reverseRec (x:xs) = (reverseRec xs) ++ [x]

reverseTailRec :: [a] -> [a]
reverseTailRec xs = revAux (xs, [])
    where  revAux ([],   accum) = accum
           revAux (y:ys, accum) = revAux (ys, y:accum)

fibRec :: Int -> Int
fibRec n =  if  n == 0 then  1
                       else if n == 1 then 1
                       else fibRec (n - 1) + fibRec (n - 2)

fibTailRec :: Int -> Int
fibTailRec n = fibAux (0, 1, 0)
  where fibAux (f1, f2, i) = if (n == i) then f2
                                         else fibAux (f2, f1+f2, i+1)
-- EXERCISE 11
-- https://wiki.haskell.org/Foldr_Foldl_Foldl'
-- Write minimal examples highlighting the differences between the three folds
-- => I wrote that in my notebook :)
--    anyway remember foldr for infinite foldable and we dont care about the order
--                    foldl' with seq to enforce strictness to not stackoverflow
