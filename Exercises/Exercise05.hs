-- Exercise 5 (Recursion over Lists)

toBeImplemented = undefined

--delDups deletes duplicates from a list
testDelDups =
  delDups [1,2,3,4,5] == [1,2,3,4,5] &&
  delDups [1,1,1,1,1] == [1] &&
  (delDups [1,2,2,4,1] == [1,2,4] || delDups [1,2,2,4,1] == [2,4,1]) &&
  delDups []          == ([] :: [Int])

delDups :: Eq a => [a] -> [a]
delDups [] = []
delDups (x:xs) | x `elem` xs = delDups xs
               | otherwise = x : delDups xs

-- removeEachSnd removes each second element from a list.
testRemoveEachSnd =
  removeEachSnd [1,2,3,4,5,6,7,8] == [1,3,5,7] &&
  removeEachSnd [1,2,3,4,5,6,7]   == [1,3,5,7] &&
  removeEachSnd [1]               == [1] &&
  removeEachSnd []                == ([] :: [Int])

removeEachSnd :: [a] -> [a]
removeEachSnd [] = []
removeEachSnd (x:xs) = x : removeEachSnd (drop 1 xs)
                     

-- makePairs pairs adjacent elements of a list
testMakePairs =
  makePairs [1,2,3,4,5,6,7,8] == [(1,2),(3,4),(5,6),(7,8)] &&
  makePairs [1,2,3,4,5,6,7]   == [(1,2),(3,4),(5,6)] &&
  makePairs [1,2]             == [(1,2)] &&
  makePairs [1]               == [] &&
  makePairs []                == ([] :: [(Int,Int)])

makePairs :: [a] -> [(a, a)]
makePairs [] = []
makePairs (x:xs)    | (length xs) < 1 = []
                    | otherwise = (x,(head xs)) : makePairs (tail xs)


testMakePairsV2 =
  makePairsV2 [1,2,3,4,5,6,7,8] == [(1,2),(3,4),(5,6),(7,8)] &&
  makePairsV2 [1,2,3,4,5,6,7]   == [(1,2),(3,4),(5,6)] &&
  makePairsV2 [1,2]             == [(1,2)] &&
  makePairsV2 [1]               == [] &&
  makePairsV2 []                == ([] :: [(Int,Int)])

makePairsV2 :: [a] -> [(a, a)]
makePairsV2 = toBeImplemented

-- halve divides a list into two lists containing each second element,
-- the first list beginning with the first,
-- the second list beginning with the second
testHalve =
  halve [1,2,3,4,5,6] == ([1,3,5], [2,4,6]) &&
  halve [1,2,3,4,5]   == ([1,3,5], [2,4]) &&
  halve [1]           == ([1], []) &&
  halve []            == ([], [] :: [Int])

halve :: [a] -> ([a], [a])
halve [] =( [], []) 
halve a =( (removeEachSnd a) , (removeEachSnd (tail a)))

-- divideList divides a list into chunks of length n each, except
-- of the last chunk, which might be shorter
-- Precondition:
--   n > 0
-- Theorem:
--   For all n > 0 and all xs: concat (divideList n xs) == xs
testDivideList =
  divideList 3 [1 .. 10] == [[1,2,3],[4,5,6],[7,8,9],[10]] &&
  divideList 3 [1 .. 9] == [[1,2,3],[4,5,6],[7,8,9]] &&
  divideList 3 [1] == [[1]] &&
  divideList 3 [] == ([] :: [[Int]])

divideList :: Int -> [a] -> [[a]]
divideList _ [] = []
divideList n x = (take n x) : divideList n (drop n x)
