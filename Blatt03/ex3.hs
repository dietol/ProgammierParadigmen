module Merge where

merge :: Ord t =>[t] -> [t] -> [t]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
	| (x<y) = x:merge xs (y:ys)
	| otherwise = y:merge (x:xs) ys 

odds = 1 : map (+2) odds
oddPrimes (p : ps) = p : (oddPrimes [q | q <- ps, q `mod` p /= 0])
primes = 2 : oddPrimes (tail odds)

primepowers :: Integer -> [Integer]
primepowers n = foldr merge [] [[p ^ i | p <- primes] | i <- [1..n]]
