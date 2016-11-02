pow1 b e 
	| (e==0) = 1
	| otherwise =  b * pow1 b (e-1)

pow2 b e
	| (e==0) = 1
	| (odd e) = b * (pow2 (pow2 b 2) ((e-1) `div` 2)) 
	| otherwise =  b * pow2 b (e-1)


pow3 b e 
	| (e<0) = error "Do not use negative exponents!"
	| (e==0) = 1
	| otherwise =  pow3Help b e b

pow3Help b e acc
	| (e==0) = 1
	| (odd e) = pow3Help (pow3Help b 2 b) ((e-1) `div` 2) acc*b
	| otherwise = pow3Help b (e-1) acc*b
	 
