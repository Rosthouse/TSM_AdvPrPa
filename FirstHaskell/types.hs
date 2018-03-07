einkaufsliste = 
    [
        (3, "Wiederstand 10kOhm"),
        (5, "Kondensator 0.1microFarad"),
        (2, "Zahnrad 38 Zaehne")
    ]

preisliste = 
    [
        ("Zahnrad 38 Zaehne", 1200),
        ("Widerstand 10kOhm", 50),
        ("Widerstand 20kOhm", 50),
        ("Kondensator 0.1microFarad", 50)
    ]

f1 :: Int -> Int
f1 x = x^2 + x + 1

f2 :: Int -> Int
f2 x = 2 * x + 1

g1 :: Int -> Int -> Int -> Int
g1 x y z = x^2 + y^2 + z^2

g2 :: Int -> Int -> Int
g2 x y = 2*x + 2*y

h1 x = (x, x, x)
h2 x = [x, x, x]
h3 x = [(x, x), (x, x)]
h4 x y = (x, y)
h5 x y = [x,y]