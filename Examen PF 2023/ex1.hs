data Expr = Var String | Val Int | Plus Expr Expr | Mult Expr Expr
    deriving (Show, Eq)

class Operations exp where
    simplify :: exp -> exp

instance Operations Expr where
    simplify (Var a) = (Var a)
    simplify (Val a) = (Val a)
    simplify (Mult exp1 exp2) = let 
                                    e1 = simplify exp1
                                    e2 = simplify exp2
                                in if (e1 == (Val 0) || e2 == (Val 0)) then (Val 0)
                                   else if e1 == (Val 1) then e2
                                        else if e2 == (Val 1) then e1
                                            else (Mult e1 e2) 
    simplify (Plus exp1 exp2) = let 
                                    e1 = simplify exp1
                                    e2 = simplify exp2
                                in if e1 == (Val 0) then e2
                                   else if e2 == (Val 0) then e1
                                        else (Plus e1 e2) 



ex1 = Mult (Plus (Val 1) (Var "x")) (Val 1)
ex2 = Plus ex1 (Val 3)
ex3 = Plus (Mult (Val 0) (Val 2)) (Val 3)
ex4 = Mult ex3 (Val 5)