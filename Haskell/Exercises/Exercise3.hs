-- Aufgabe 1 List sugaring
s1 = 1 : 2 : 3 : [4]
s1s = [1, 2, 3, 4]
s1e = s1 == s1s

s2 = 1 : 2 : [3, 4]
s2s = [1, 2, 3, 4]
s2e = s2 == s2s

s3 = (1 : 2 : []) : ( 3 : [] ) : []
s3s = [[1, 2], [3]]
s3e = s3 == s3s

s4 = (1, 2) : (3, 4) : [(5, 6)]
s4s = [(1,2), (3, 4), (5, 6)]
s4e = s4 == s4s

s5 = [] : []
s5s = [[]]
-- s5e = s5 == s5s

s6 = [] : [] : []
s6s = [[], []]

s7 = ([] : [] ) : []
s7s = [[[]]]

s8 = (([] : []) : []) : []
s8s = [[[[]]]]

s9 = 'a' : 'b' : []
s9s = "ab"
s9e = s9 == s9s

-- Aufgabe 2 List desugaring
d1 = [1, 2, 3] == 1 : 2 : 3 : []
d2 = [[1, 2], [], [3, 4]] == (1 : 2 : []) : [] : (3 : 4 : []) : []
-- d3 = [[], ["a"], [[]]] == [] : ('a' : []) : ([[]]) : []

-- Aufgabe 3 Pattern Matching
f1 (x : y : z) = (x, y, z)
f2 [x, y] = (x, y)
f3 (x : y : []) = (x, y)

a11 = f1 []
a21 = f2 []
a32 = f3 []

a12 = f1 [1]
a22 = f2 [1]
a32 = f3 [1]

a13 = f1 [1, 2]
a23 = f2 [1, 2]
a33 = f3 [1, 2]