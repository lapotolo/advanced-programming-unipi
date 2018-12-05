data ListBag a = LB [(a, Int)] deriving (Show, Eq)

-- A ListBag is well-formed if it does not contain two pairs 
-- (v, k) and  (v', k') with v = v'.

empty :: ListBag a
empty = LB []

singleton :: (Eq a) => a -> ListBag a
singleton v = LB [(v, 1)]

singletonK :: (Eq a) => a -> Int -> ListBag a
singletonK v k = LB [(v, k)]

toList :: (Eq a) => ListBag a -> [a]
toList (LB []) = [] 
toList LB((x,k):xs) = replicate k x ++ toList xs 

-- fromList lst, returning a ListBag containing all and only the elements of lst, each with the right multiplicity
--fromList :: (Eq a) => [a] -> ListBag a
--fromList [] = LB []
--fromList xs = foldl insertBagList empty xs 
--
--insertBagList :: (Eq a) => ListBag a -> a -> ListBag a
--insertBagList [] v         = (LB []) : (singleton v)
--insertBagList ((x,k):xs) v = if (==) x v 
--                              then LB[(x, k+1)] : xs  
--                              else insertBagList xs v 

-- wf :: (Eq a) => ListBag a -> Bool
-- wf LB [] = True
-- wf ((x,k):xs) = (&&)  (wf xs) 