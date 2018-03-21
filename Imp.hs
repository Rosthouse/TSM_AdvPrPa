data ArithExpr
    = LitAExpr Int -- Literal Arithmetic Expression
    | DyaAExpr ArithOperator ArithExpr ArithExpr -- Dya
    deriving Show

data ArithOperator 
    = Times | Div | Mod | Plus | Minus
    deriving Show

-- (2 + 3 ) * 4
ae1, ae2 :: ArithExpr
ae1 = DyaAExpr Plus ( LitAExpr 2 ) (LitAExpr 3) 
ae2 = DyaAExpr Times ae1 (LitAExpr 4)

evalAExpr :: ArithExpr -> Int -- Semantische Funktion
evalAExpr (LitAExpr c) =  c
evalAExpr (DyaAExpr opr exp1 exp2) = 
    evalAOperator opr val1 val2
    where   val1 = evalAExpr exp1
            val2 = evalAExpr exp2


evalAOperator :: ArithOperator -> (Int -> Int -> Int)
evalAOperator Times = (*)
evalAOperator Div   = div
evalAOperator Mod   = mod
evalAOperator Plus  = (+)
evalAOperator Minus = (-)