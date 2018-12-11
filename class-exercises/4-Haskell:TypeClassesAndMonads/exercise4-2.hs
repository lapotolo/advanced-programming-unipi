data Expr a = Const a
            | Sum (Expr a) (Expr a)
            | Mul (Expr a) (Expr a)
            | Div (Expr a) (Expr a)
            | Id Var
            deriving (Show)

data Var = X | Y | Z
         deriving (Show)

test_expr1 = (Sum
                (Mul (Const 2) (Const 3))
                (Const 4)
             )
             
test_expr2 = (Sum -- safeEval = 13
                (Mul
                    (Div  (Const 6) (Const 3) )
                    (Div  (Const 5) (Const 5) )
                )
                (Sum
                    (Const 5)
                    (Const 6)
                )
             )



-- EXERCISE 1
eval :: Expr Integer -> Integer
eval (Const op)    = op
eval (Sum op1 op2) = (+) (eval op1) (eval op2)
eval (Mul op1 op2) = (*) (eval op1) (eval op2)


-- EXERCISE 2
-- helper function used to extract the value from  a "Maybe box"
fromJust :: Maybe a -> a
fromJust Nothing  = undefined
fromJust (Just x) = x

safeEval :: Expr Integer -> Maybe Integer
safeEval (Const op)    = Just op
safeEval (Sum op1 op2) = Just ((+) (fromJust(safeEval op1)) (fromJust(safeEval op2)))
safeEval (Mul op1 op2) = Just ((*) (fromJust(safeEval op1)) (fromJust(safeEval op2)))
safeEval (Div op1 op2) =
    let op1' = safeEval op1
        op2' = safeEval op2
     in if op2' == Just 0
            then Nothing
            else Just ((quot) (fromJust op1') (fromJust op2'))

  -- | (==) 0 (safeEval op2)        = Nothing
  -- | (&&) ((==) (safeEval op1) 0)
  --        ((==) (safeEval op2) 0) = Nothing
  -- | otherwise                = Just ((quot) (safeEval op1) (safeEval op2))


-- EXERCISE 3
instance Functor Expr where
  fmap f (Const op)    = Const (f op)
  fmap f (Sum op1 op2) = Sum (fmap f op1) (fmap f op2)
  fmap f (Mul op1 op2) = Mul (fmap f op1) (fmap f op2)
  fmap f (Div op1 op2) = Div (fmap f op1) (fmap f op2)

-- EXERCISE 4

instance Foldable Expr where
  foldMap f (Const op)    = op
  foldMap f (Sum op1 op2) =
  foldMap f (Mul op1 op2) =
  foldMap f (Div op1 op2) =
