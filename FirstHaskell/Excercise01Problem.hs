-- Exercise 1
-- Develop some functions to work with two-dimensional vectors

-- a type for two-dimensional vectors
type Vec = (Double, Double)

-- the zero vector
zeroVec :: Vec
zeroVec = (0, 0)

-- some example vectors
a, b, c, d :: Vec
a = (3, 0)
b = (0, 4)
c = (sqrt2, sqrt2)
      where sqrt2 = sqrt 2
d = (3, 4)

-- lengthVec computes the length of a vector
-- example: lengthVec c == 2
lenghtVec :: (Vec) -> Double
lenghtVec (x, y) = 
    sqrt( x^2 + y^2)

-- negVec negates a vestor
-- example: negVec d == (-3, -4)
negVec :: (Vec) -> (Vec)
negVec = scaleVec (-1.0)



-- addVec adds two vectors
-- example: a `addVec` b == d
addVec :: Vec -> Vec -> Vec
addVec (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)



-- subVec subtracts two vectors
-- example: a `subVec` b == (3, -4)
-- implement this function using negVec and addVec
subVec :: Vec -> Vec -> Vec
subVec v1 v2 = addVec v1 (negVec v2:t )


-- distance computes the distance between two vectors
-- example: distance a d == 4
-- implement this function using subVec and lengthVec
distance :: Vec -> Vec -> Double
distance (x1, y1) (x2, y2) = sqrt ((x2 -x1)^2 + (y2-y1)^2)


-- scales a vector with a factor
-- example: scaleVec c (sqrt 2) == (2, 2)
--   (at least approximately)
scaleVec :: Double -> Vec -> Vec
scaleVec a (x, y) = (a*x, a*y)

