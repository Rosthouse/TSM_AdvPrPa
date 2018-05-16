module Demo where

type VAR = String
type STATE = VAR -> Int

state1 :: STATE
state1 "m" = 17
state1 "n" =  5
state1 "q" =  1
state1 "r" = 12
state1 _ = 0

readS :: (STATE, VAR) -> Int
readS (state, var) = state (var)

updateS :: (STATE, (VAR, Int)) -> STATE
updateS (state, (var, val)) (var') =
  if var' == var then val
  else state(var')
