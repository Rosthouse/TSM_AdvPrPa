(==>) :: Bool -> Bool -> Bool
False ==> _ = True
True  ==> p = p

infix 2 ==>

(<=>) :: Bool -> Bool -> Bool
p <=> q = p == q
--False <=> False = True
--False <=> True  = False
--True <=> False  = False
--True <=> True   = True

formula1 p q = (p <=> q) <=> ((p ==> q) && (q ==> p))

formula2 p q = (p ==> q ) <=> (not p || q)

check :: (Bool -> Bool -> Bool) -> Bool
check formula = 
    formula False False &&
    formula True False &&
    formula False True &&
    formula True True

check1 = check formula1
check2 = check formula2

checkV2 formula =
    and [formula p q | p <-[False, True], q <- [False, True] ] 