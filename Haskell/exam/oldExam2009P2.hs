f x y | x == y = "ok"
      | otherwise = "ko"


delDups :: Eq a => [a] -> [a]
delDups [] = []
delDups (x:xs) = if x `elem` cleaned then cleaned else x:cleaned
 where 
    cleaned = delDups xs