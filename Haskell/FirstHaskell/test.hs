double x = x + x

quadruple x = double x + double x

factorial n = product [1..n]

average ns = sum ns `div` length ns

a = b + c
    where
        b =  1
        c = 2

n = a `div` length xs
    where
        a = 10
        xs = [1,2,3,4,5]

add :: (Int, Int) -> Int
add(x,y) = x + y

zeroton :: (Int) -> [Int]
zeroton(n) = [0..n]