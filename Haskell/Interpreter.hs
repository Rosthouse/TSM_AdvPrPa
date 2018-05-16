type Value = Int
type Ident = String

type State = Ident -> Value


readS :: State ->  Ident -> Value
readS state ident = state ident

updateS :: State -> (Ident, Value) -> State
updateS s (ident, val) = 
    \ident' ->  if ident' == ident then val
                else s ident'


data ArithExpr
    = LitAExpr Int -- Literal Arithmetic Expression
    | IdAExpr Ident
    | DyaAExpr ArithOperator ArithExpr ArithExpr -- Dya
    deriving Show

data ArithOperator 
    = Times 
    | Div 
    | Mod 
    | Plus 
    | Minus
    deriving Show

evalAExpr :: ArithExpr -> State -> Int -- Semantische Funktion
evalAExpr (LitAExpr c) _ =  c
evalAExpr (IdAExpr ident) state = readS state ident
evalAExpr (DyaAExpr opr exp1 exp2) state = 
    operation val1 val2
    where   operation = evalAOperator opr
            val1 = evalAExpr exp1 state
            val2 = evalAExpr exp2 state


evalAOperator :: ArithOperator -> (Value -> Value -> Int)
evalAOperator Times = (*)
evalAOperator Div   = div
evalAOperator Mod   = mod
evalAOperator Plus  = (+)
evalAOperator Minus = (-)

data RelOperator 
    = Equals
    | Less
    | LessEq
    | Greater
    | GreaterEq
    deriving Show

evalROpr :: RelOperator -> (Value -> Value -> Bool)
evalROpr Equals = (==)
evalROpr Less = (<)
evalROpr LessEq = (<=)
evalROpr Greater = (>)
evalROpr GreaterEq = (>=)

data BoolExpr
    = RelBExpr RelOperator ArithExpr ArithExpr
    deriving Show


evalBExpr :: BoolExpr -> State -> Bool
evalBExpr (RelBExpr opr expr1 expr2) state =
        operation val1 val2
    where   operation = evalROpr opr
            val1 = evalAExpr expr1 state
            val2 = evalAExpr expr2 state

data Command
    = AssiCmd Ident ArithExpr
    | CpsCmd [Command]
    | WhileCmd BoolExpr Command
    | CondCmd BoolExpr Command Command
    deriving Show

interCmd :: Command -> State -> State
interCmd (AssiCmd ident expr) state = 
    updateS state (ident, val)
    where
        val = evalAExpr expr state 

interCmd (CpsCmd cmds) state = 
    foldl (flip interCmd) state cmds 

interCmd (WhileCmd guard repetend) state 
    | evalBExpr guard state =
       interCmd (WhileCmd guard repetend) state'
    | otherwise = state
       where state' = interCmd repetend state


interCmd (CondCmd branch ifBranch elseBranch) state  
    | evalBExpr branch state = interCmd ifBranch state
    | otherwise = interCmd elseBranch state

state175 "m" = 17
state175 "n" = 5
state175 _ = 0

-- stateOut = interCmd divProg state175

-- (2 + 3 ) * 4
ae1, ae2 :: ArithExpr
ae1 = DyaAExpr Plus ( LitAExpr 2 ) (LitAExpr 3) 
ae2 = DyaAExpr Times ae1 (LitAExpr 4)

-- (q + 1)
aeqi = DyaAExpr Plus (IdAExpr "q") (LitAExpr 1)
-- (r - n)
aern = DyaAExpr Minus (IdAExpr "r") (IdAExpr "n")