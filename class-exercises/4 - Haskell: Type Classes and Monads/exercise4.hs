data Expr a = Const a
            | Sum (Expr a) (Expr a)
            | Sub (Expr a) (Expr a)
            | Mul (Expr a) (Expr a)
            | Div (Expr a) (Expr a)

eval :: (Num a) => Expr a -> a
eval (Const op) = op
eval (Sum op1 op2) = (+) (eval op1) (eval op2)
eval (Sub op1 op2) = (-) (eval op1) (eval op2) -- bonus
eval (Mul op1 op2) = (*) (eval op1) (eval op2)

test_expr1 = (Sum (Mul (Const 2) (Const 3)) (Const 4))
test_expr2 = (Sub (Const 5) (Const 3) )


handleDivision :: (Num a) => Expr a -> Maybe a
handleDivision (Div _ 0) = Nothing
handleDivision  (Div 0 0) = Nothing
handleDivision (Div op1 op2) = (/) (safeEval op1) (safeEval op2)

safeEval :: (Num a) => Expr a -> Maybe a
safeEval (Const op) = Just op
safeEval (Sum op1 op2) = (+) (safeEval op1) (safeEval op2)
safeEval (Sub op1 op2) = (-) (safeEval op1) (safeEval op2)
safeEval (Mul op1 op2) = (*) (safeEval op1) (safeEval op2)
safeEval (Div op1 op2) = handleDivision
-- safeEval (Div op1 op2) = (/) (safeEval op1) (safeEval op2)
