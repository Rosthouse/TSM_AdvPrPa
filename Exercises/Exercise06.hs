-- Exercise 6 (Higher-Order Functions)

import Prelude hiding (flip, curry, uncurry)

toBeImplemented = undefined

-- flip f takes its (first) two arguments in the reverse order of f
testFlip = flip take [1,2,3,4,5] 3 == [1,2,3]

flip :: (a -> b -> c) -> (b -> a -> c)
flip a b c = a c b

-- curry converts a function on pairs to a curried function
testCurry = curry (\(x,y) -> x + y) 3 4 == 7

curry :: ((a, b) -> c) -> (a -> b -> c)
curry a b c = a (b, c)

-- uncurry converts a curried function to a function on pairs
testUncurry = uncurry (\x y -> x + y) (3, 4) == 7

uncurry :: (a -> b -> c) -> ((a, b) -> c)
uncurry a (b, c) = a b c

-- implement reverse using foldr and (++)
reverseR :: [a] -> [a]
reverseR = foldr (\ x  accu -> accu ++ [x]) []

-- implement reverse using foldl, (:), and flip
reverseL x = foldl (flip (:)) []

-- revAppend prepends the first list in reverse order before the second list
testRevAppend = revAppend [3,2,1] [4,5,6] == [1,2,3,4,5,6]
-- implement revAppend using foldl and flip

revAppend :: [a] -> [a] -> [a]
revAppend = toBeImplemented
