zap :: [a] -> [b] -> [(a,b)]
zap [] _ = []
zap _ [] = []
zap (x:xs) (y:ys) = (x, y):zap xs ys

unzap :: [(a, b)] ->([a], [b])
unzap [] = ([], [])
unzap ((x, y):xs) = (x:left, y:right)
where 
    right = 