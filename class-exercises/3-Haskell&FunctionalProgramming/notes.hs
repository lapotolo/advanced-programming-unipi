bmiTell :: (RealFloat a) => a -> a -> String  
bmiTell weight height  
  | weight / height ^ 2 <= 18.5 = "You're underweight, you emo, you!"  
  | weight / height ^ 2 <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"  
  | weight / height ^ 2 <= 30.0 = "You're fat! Lose some weight, fatty!"  
  | otherwise                   = "You're a whale, congratulations!"  


-- bmiTellWhere :: (RealFloat a) => a -> a -> String  
-- bmiTellWhere weight height  
--   | bmi <= skinny = "You're underweight, you emo, you!"  
--   | bmi <= normal = "You're supposedly normal. Pffft, I bet you're ugly!"  
--   | bmi <= fat    = "You're fat! Lose some weight, fatty!"  
--   | otherwise     = "You're a whale, congratulations!"  
--   where bmi = weight / height ^ 2
--         (skinny, normal, fat) = (18.5, 25.0, 30.0)

calcBmis :: (RealFloat a) => [(a, a)] -> [a]  
calcBmis xs = [bmi w h | (w, h) <- xs]  
        where bmi weight height = weight / height ^ 2               
-- let bindings are expressions themselves. where bindings are just syntactic constructs
-- let bindings do not spawn across guards
cylinder :: (RealFloat a) => a -> a -> a  
cylinder r h = 
    let sideArea = 2 * pi * r * h  
        topArea = pi * r ^2  
    in  sideArea + 2 * topArea

-- case can be used everywhere!
describeList :: [a] -> String  
describeList xs = "The list is " ++ case xs of [] -> "empty."  
                                               [x] -> "a singleton list."   
                                               xs -> "a longer list."  
describeList' :: [a] -> String  
describeList' xs = "The list is " ++ what xs  
             where what [] = "empty."  
                   what [x] = "a singleton list."  
                   what xs = "a longer list."  

maximum' :: (Ord a) => [a] -> a  
maximum' [] = error "maximum of empty list"  
maximum' [x] = x  
maximum' (x:xs)   
  | x > maxTail = x  
  | otherwise = maxTail  
  where maxTail = maximum' xs  

maximum'' :: (Ord a) => [a] -> a  
maximum'' [] = error "maximum of empty list"  
maximum'' [x] = x  
maximum'' (x:xs) = max x (maximum' xs)  

-- WE USE GUARDS INSTEAD OF PATTERNS  SINCE WE ARE TESTING FOR A BOOLEAN CONDITION
replicate' :: (Num i, Ord i) => i -> a -> [a]
replicate' n x
    | n <= 0    = []
    | otherwise = x : replicate' (n-1) x
      

take' :: (Num i, Ord i) => i -> [a] -> [a]  
take' n _  
  | n <= 0   = []  
take' _ []     = []  
take' n (x:xs) = x : take' (n-1) xs  

reverse' :: [a] -> [a]
reverse'   [] = []
reverse' (x:xs) = reverse' xs ++ [x] 

repeat' :: a -> [a]  
repeat' x = x:repeat' x

zip' :: [a] -> [b] -> [(a,b)]
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x,y) : zip' xs ys

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]  
zipWith' _ [] _ = []  
zipWith' _ _ [] = []  
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys 

flip' :: (a -> b -> c) -> b -> a -> c  
flip' f y x = f x y

-- ghci> flip' zip [1,2,3,4,5] "hello"  
-- [('h',1),('e',2),('l',3),('l',4),('o',5)]  
-- ghci> zipWith (flip' div) [2,2..] [10,8,6,4,2]  
-- [5,4,3,2,1]  

map' :: (a -> b) -> [a] -> [b]
map' f [] = []
map' f (x:xs) = f x : map' xs

filter' :: (a -> Bool) -> [a] -> a
filter' _ [] = []
filter' pred (x:xs)
  | pred x    = x : filter' pred xs
  | otherwise = filter' pred xs

-- quicksortFilter :: (Ord a) => [a] -> [a]    
-- quicksortFilter [] = []    
-- quicksortFilter (x:xs) =     
--   let smallerSorted = quicksortFilter (filter' (<=x) xs)  
--       biggerSorted = quicksortFilter f(ilter' (>x) xs)   
--   in  smallerSorted ++ [x] ++ biggerSorted  

-- ORDER OF EVALUATION foldr f z [3,4,5,6] =f 3 (f 4 (f 5 (f 6 z)))
-- (+) 3 ((+) 4 ((+) 5 ((+) 6 0)))

-- ORDER OF EVALUATION foldl

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

-- mapWithFoldr f xs = foldr (\x acc -> f x : acc) [] xs

-- mapWithFoldl f xs = foldl (\x acc -> f x : acc) [] xs