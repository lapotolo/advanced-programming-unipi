data ListBag a = LB [(a, Int)]
  deriving (Show, Eq)


-- A ListBag is well-formed if it does not contain two pairs (v, k) and (v', k') with v = v'.
wf :: (Eq a) => ListBag -> Bool
wf ()