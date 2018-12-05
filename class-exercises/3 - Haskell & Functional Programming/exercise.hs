foldr' f z []     = z
foldr' f z (x:xs) = x `f` foldr f z xs

foldl' f z []     = z
foldl' f z (x:xs) = let z' = z `f` x 
                    in seq z' $ foldl' f z' xs

elem' :: (Eq a) => a -> [a] -> Bool
elem' _ [] = False
elem' v (x:xs) = (||) ((==) v x) (elem' v xs)

capital :: String -> String  
capital "" = "Empty string, whoops!"  
capital all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]  
reverseQuadratic [] = []
reverseQuadratic (x:xs) = (reverseQuadratic xs) ++ [x]

-- reverseLinear xs =
--     let rev ([], accum) = accum
--         rev (y:ys, accum) = rev (ys y:accum)
--     in rev (xs, [])

    -- EXERCISE 1
-- Recursive implementation
myReplicateRec :: Integer -> Integer -> [Integer]
myReplicateRec 0 _ = []
myReplicateRec n v = v : myReplicateRec (n-1) v


-- Combinator implementation map filter foldl/r
--myReplicateCom :: Integer -> Integer -> [Integer]
--myReplicateCom n v = foldl (const v) [] [1..n]