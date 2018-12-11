data Tree a = Empty | Leaf a | Node (Tree a) a (Tree a) deriving Show

test_tree1 = Leaf 5
test_tree2 = Node
                 (Leaf 6)
                 5
                 (Leaf 4)
-- a suitable instance would be

instance Foldable Tree where
   foldMap f Empty = mempty
   foldMap f (Leaf x) = f x
   foldMap f (Node l k r) = foldMap f l `mappend` f k `mappend` foldMap f r

-- This is suitable even for abstract types, as the monoid is assumed to satisfy
-- the monoid laws. Alternatively, one could define foldr:

-- instance Foldable Tree where
--   foldr f z Empty = z
--   foldr f z (Leaf x) = f x z
--   foldr f z (Node l k r) = foldr f (f k (foldr f z r)) l