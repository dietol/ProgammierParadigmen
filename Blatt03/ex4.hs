module Intersect where

intersect :: (Ord t) =>[t] -> [t] -> [t]
intersect xs [] = []
intersect [] ys = []
intersect (x:xs) (y:ys)
	| (x<y) = intersect xs (y:ys)
	| (x>y) = intersect (x:xs) ys
	| otherwise = x:intersect xs ys

intersectAll :: (Ord t) =>[[t]] -> [t]
intersectAll xs = foldr intersect (head xs) xs

commonMultiples :: Int -> Int -> Int -> [Int]
commonMultiples a b c = intersectAll [[x,x*2..] | x <- [a,b,c]] 
