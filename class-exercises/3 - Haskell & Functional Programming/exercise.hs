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