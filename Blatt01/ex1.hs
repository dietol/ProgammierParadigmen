module Arithmetik where

pow1 b e 
	| (e<0) = error "no negative exponents with this function"
	| (e==0) = 1
	| otherwise = b * pow1 b (e-1)

pow2 b e
	| (e<0) = error "no negative exponents with this function"
        | (e==0) = 1
	| (e `mod` 2 == 0) = pow2 (b*b) (e `div` 2)
	| otherwise = b * pow2 (b*b) (e `div` 2)

 
pow3 b e 
	| (e<0) = error "no negative exponents with this function"
	| (e==0) = 1
	| otherwise = pow3rec b e 1

pow3rec b e acc 
	| (e==0) = acc
	| (e `mod` 2 == 0) = pow3rec (b*b) (e `div` 2) acc
        | otherwise =  pow3rec (b*b) (e `div` 2) acc*b

root e r
	| (r<0) = error "only positive roots valid"
	| (r==0) = 0
	| otherwise = rootHelp e r 0 r

rootHelp e r a b
	| ((b-a) < 1) = error "something went wrong"
	| ((b-a) == 1) = a
	| ((pow3 (rootInt a b) e) > r) = rootHelp e r a (rootInt a b)
	| otherwise = rootHelp e r (rootInt a b) b 
	
rootInt a b = a + ((b-a) `div` 2) 

isPrime n
	| (n<0) = error "no negative numbers"
	| (n==1) = False
	| (n==2) = True
	| (n==3) = True
	| (n==4) = False
	| otherwise = isPrimeHelp n 2 (root 2 n) True

isPrimeHelp n a b acc
	| (a==b) =  (n `mod` a /= 0) && acc
	| otherwise = isPrimeHelp n (a+1) b ((n `mod` a /= 0) && acc)
		
