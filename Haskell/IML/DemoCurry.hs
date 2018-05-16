module DemoCurry where

type Ident = String
type Value = Int
type State = Ident -> Value

state1 :: State
state1 "m" = 17
state1 "n" =  5
state1 "q" =  1
state1 "r" = 12
state1 _ = 0

readS :: State -> Ident -> Value
readS s ident = s ident

updateS :: State -> (Ident, Value) -> State
updateS s (ident, val) ident' | ident' == ident = val
                              | otherwise = s ident'
