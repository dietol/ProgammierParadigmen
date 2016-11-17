type Polynom = [Double]

add:: Polynom -> Polynom -> Polynom
add [] [] = []
add [] (y:ys) = (y:ys)
add (x:xs) [] = (x:xs)
add (x:xs) (y:ys) = (x+y):add xs ys

eval :: Polynom -> Double -> Double
eval xs y = foldr (\x acc -> (acc+x)*y) 0 xs / y

deriv :: Polynom -> Polynom
deriv xs = zipWith (*) [0..] xs
 
