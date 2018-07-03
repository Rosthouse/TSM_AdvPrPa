data IntInf = MinusInf
            | Normal Int
            | PlusInf
            deriving Show

less :: IntInf -> IntInf -> Bool
less _ MinusInf = False
less MinusInf (Normal x) = True
less (Normal x) (Normal y) = x < y
less PlusInf (Normal x) = True
less PlusInf PlusInf = True
less _ PlusInf = False


minInf :: IntInf -> IntInf -> IntInf
minInf a b = if a `less` b then a else b 

minListInf :: [IntInf] -> IntInf
minListInf [] = PlusInf
minListInf (x:xs) = minInf (minListInf xs) x