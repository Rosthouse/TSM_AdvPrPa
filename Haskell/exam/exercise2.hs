type Poly = [(Integer, Integer)]

pIsRep :: Poly -> Bool
pIsRep [] = False
pIsRep ((e, x):[])= if e >= 0 then True else False
pIsRep ((e1, x1):(e2,x2):xs) = if hasNonNegativeExponent && isMonotonicDescending 
    then pIsRep ((e2,x2):xs)
    else False
    where
        hasNonNegativeExponent = e1 >= 0 && e2 >= 0
        isMonotonicDescending = e1 > e2

pEval :: Poly -> Integer -> Integer
pEval [] x = 0
pEval ((e, n):[]) x = n * x^e
pEval((e,n):xs) x = n * x^e + pEval xs x

pSum :: Poly -> Poly -> Poly
pEval a b = zip a b

