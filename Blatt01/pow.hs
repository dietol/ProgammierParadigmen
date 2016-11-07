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

root e r 
	| e<0 = error "Do not use negative exponents!"
        | e==0 && r /= 1 = error "Anything to the power of 0 is 1!"
	| e==0 = error "Not calculatable because everything to the power of 0 is 1!"
	| r < 0 = error "A potenz is always greater then 0"
	| otherwise = rootHelp e r 0 r


rootHelp e r a b
        | (b-a == 1) = a
        | (b-a < 0) = error "something goes wrong. b is less then a..."
        | otherwise = rootHelp e r (leftHelp e r a b) (rightHelp e r a b)

leftHelp e r a b
	| even (b-a) && (((a + (div (b-a) 2))^e) <= r) = a + (div (b-a) 2)
	| even (b-a) && (((a + (div (b-a) 2))^e) > r) = a
	| odd (b-a) && (((a + (div (b-a-1) 2))^e) <= r) = a + (div (b-a-1) 2)
        | odd (b-a) && (((a + (div (b-a-1) 2))^e) > r) = a
        | otherwise = error "uncatched pattern. stupid!"

rightHelp e r a b
        | even (b-a) && (((a + (div (b-a) 2))^e) <= r) = b
        | even (b-a) && (((a + (div (b-a) 2))^e) > r) = a + (div (b-a) 2)
        | odd (b-a) && (((a + (div (b-a-1) 2))^e) <= r) = b
        | odd (b-a) && (((a + (div (b-a-1) 2))^e) > r) = a + (div (b-a-1) 2)
        | otherwise = error "uncatched pattern. stupid!"

