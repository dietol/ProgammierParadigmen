module Collatz where

collatz :: Int -> [Int]
collatz a = iterate helper a
	where helper b 
		| b `mod` 2 == 0 = b `div` 2
		| otherwise = 3*b+1

num :: Int -> Int
num a = fst $ head $ filter (\(x,y) -> y==1) $ zipWith (,) [0..] $ collatz a

maxNum :: Int -> Int -> (Int,Int)
maxNum a b = foldr (\x (y,z) -> if (num x)>z then (x,num x) else (y,z)) (a,(num a)) [a..b]
