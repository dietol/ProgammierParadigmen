type Polynom = [Double]

add:: Polynom -> Polynom -> Polynom
add [] [] = []
add [] (y:ys) = (y:ys)
add (x:xs) [] = (x:xs)
add (x:xs) (y:ys) = (x+y):add xs ys
