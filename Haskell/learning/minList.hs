minCur :: Ord a => a -> a -> a
minCur x y = if x > y then y else x

minList1 :: Ord a => [a] -> a
minList1 (x:[]) = x
minList1 (x:xs) = minCur x (minList1 xs)

minList2 :: Ord a => [a] -> a
minList2 (x:[]) = x
minList2 (x:y:xs) = minList2 (minVal:xs)
                    where 
                        minVal = minCur x y