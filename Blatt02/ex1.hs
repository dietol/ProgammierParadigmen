module Polynom where

type Polynom = [Double]

add :: Polynom -> Polynom -> Polynom
add [] ys = ys
add xs [] = xs
add (x:xs) (y:ys) = (x+y):add xs ys 

eval :: Polynom -> Double -> Double
eval xs y = foldr (\x acc -> y*acc+x) 0 xs

deriv :: Polynom -> Polynom
deriv [] = []
deriv (x:xs) = zipWith (*) xs [1..]

