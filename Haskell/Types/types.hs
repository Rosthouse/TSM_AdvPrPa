data Answer = Yes | No | Unknown
    deriving Show

data Shape = Circ Float 
            | Rect Float Float
    deriving Show

square :: Float -> Shape
square n = Rect n n

area :: Shape -> Float
area (Circ r) = pi * r^2

data Nat = Zero | Succ Nat
    deriving Show

nat2int :: Nat -> Int
nat2int Zero = 0
nat2int (Succ n) = 1 + nat2int n

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat n = Succ (int2nat (n-1))

add :: Nat -> Nat -> Nat
add Zero n = n
add (Succ m) n = Succ (add m n)