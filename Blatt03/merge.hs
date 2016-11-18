merge :: Ord t => [t] -> [t] -> [t]
merge [] [] = []
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
	| x < y = mergeRek xs (y:ys) [x]
	| otherwise = mergeRek (x:xs) ys [y]

mergeRek [] [] zs = zs
mergeRek [] ys zs = zs ++ ys
mergeRek xs [] zs = zs ++ xs
mergeRek (x:xs) (y:ys) zs
	| x < y = mergeRek xs (y:ys) (zs ++ [x])
	| otherwise = mergeRek (x:xs) ys (zs ++ [y])

primes = [1..5]

primepowers n = foldl (\acc x -> merge x acc) [] [[y ^ i | i <- [1..n]]| y <- primes]
