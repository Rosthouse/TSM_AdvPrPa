module Interpreter where

import AbsSyn

type Value = Int
type State = Ident -> Value

iniState :: State
iniState = \ident -> error "internal error initial state"

readS :: State -> Ident -> Value
readS s ident = s ident

updateS :: State -> (Ident, Value) -> State
updateS s (ident, val) ident' | ident' == ident = val
                              | otherwise = s ident'

evalAOpr :: ArithOperator -> (Value -> Value -> Value)
evalAOpr Times = (*)
evalAOpr Div = div
evalAOpr Mod = mod
evalAOpr Plus = (+)
evalAOpr Minus = (-)

evalROpr :: RelOperator -> (Value -> Value -> Bool)
evalROpr Less = (<)
evalROpr GreaterEq = (>=)
evalROpr Equal = (==)
evalROpr NotEq = (/=)
evalROpr Greater = (>)
evalROpr LessEq = (>=)

evalBOpr :: BoolOperator -> (Bool -> Bool -> Bool)
evalBOpr CondAnd = (&&)
evalBOpr CondOr = (||)

evalAExpr :: ArithExpr -> State -> Value
evalAExpr (LitAExpr c) _ = c
evalAExpr (IdAExpr ident) state = readS state ident
evalAExpr (DyaAExpr opr expr1 expr2) state =
    operation val1 val2
  where
    operation = evalAOpr opr
    val1 = evalAExpr expr1 state
    val2 = evalAExpr expr2 state

evalBExpr :: BoolExpr -> State -> Bool
evalBExpr (LitBExpr b) _ = b
evalBExpr (RelBExpr opr expr1 expr2) state =
    operation val1 val2
  where
    operation = evalROpr opr
    val1 = evalAExpr expr1 state
    val2 = evalAExpr expr2 state
evalBExpr (NegBExpr expr) state =
  not (evalBExpr expr state)
evalBExpr (DyaBExpr opr expr1 expr2) state =
    operation val1 val2
  where
    operation = evalBOpr opr
    val1 = evalBExpr expr1 state
    val2 = evalBExpr expr2 state

exDups :: Eq a => [a] -> Bool
exDups (x : xs) = x `elem` xs || exDups xs
exDups [] = False

checkCmd :: Command -> Bool
checkCmd (MultiAssiCmd idents exprs) =
  not (exDups idents) && length idents >= 1 && length idents == length exprs

interCmd :: Command -> State -> State
interCmd SkipCmd state = state
interCmd (AssiCmd ident expr) state =
    updateS state (ident, val)
  where
    val = evalAExpr expr state
interCmd (MultiAssiCmd idents exprs) state = state'
  where
    vals = map (flip evalAExpr state) exprs
    binds = zip idents vals
    state' = foldl updateS state binds
interCmd (CpsCmd cmds) state =
  foldl (flip interCmd) state cmds
  --foldr interCmd state (reverse cmds)
interCmd (CondCmd guard thenCmd elseCmd) state
  | evalBExpr guard state = interCmd thenCmd state
  | otherwise = interCmd elseCmd state
interCmd (WhileCmd guard repCmd) state
  | evalBExpr guard state =
      interCmd (WhileCmd guard repCmd) state'
  | otherwise = state
      where
        state' = interCmd repCmd state
-- it should be statically checked that there is no assignment to ident in cmd
-- of the ForCmd
interCmd (ForCmd ident fromExpr toExpr cmd) state = loop state'
  where
    fromValue = evalAExpr fromExpr state
    toValue = evalAExpr toExpr state
    state' = updateS state (ident, fromValue)
    loop state = if counter <= toValue then loop state'' else state
      where
        counter = readS state ident
        state' = interCmd cmd state
        state'' = updateS state' (ident, counter + 1)

interpreter :: Command -> State -> State
interpreter = interCmd
