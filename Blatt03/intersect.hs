intersectU :: (Ord t) => [t] -> [t] -> [t]
intersectU xs ys = filter (`isInSec` ys) xs
		where isInSec x zs= x `elem` zs

intersectAll :: (Ord t) => [[t]] -> [t]
intersectAll (x:xs) = foldl (\acc y -> intersect acc y) x xs

intersect :: (Ord t) => [t] -> [t] -> [t]
intersect [] _ = []
intersect _ [] = []
intersect (x:xs) (y:ys)
	| x<y = intersect xs (y:ys)
	| x>y = intersect (x:xs) ys
	| otherwise = [x] ++ (intersect xs ys) 

commonMultiples :: Int -> Int -> Int -> [Int]
commonMultiples a b c = intersectAll [[a,a*2..],[b,b*2..],[c,c*2..]]

