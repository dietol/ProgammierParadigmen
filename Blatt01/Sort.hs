insert n [] = [n]
insert n (x:xs) = insertRek n [] (x:xs)

insertRek n (x:xs) (y:ys)
	| (y<n) = insertRek n (x:xs ++ [y]) ys
	| (y>=n) =  x:xs ++ n:y:ys 

insertRek n [] (y:ys)
	| (y<n) = insertRek n [y] ys
        | (y>=n) =  n:y:ys

insertRek n (x:xs) [] = x:xs ++ [n]

insertSort (x:xs) = insertSortLin xs [x]

insertSortLin [] (y:ys) = y:ys
insertSortLin (x:[]) (y:ys) = insert x (y:ys)	
insertSortLin (x:xs) (y:ys) = insertSortLin  xs (insert x (y:ys))


merge [] [] = []
merge [] (y:ys) = y:ys
merge (x:xs) [] = x:xs
merge (x:xs) (y:ys) 
	| (x<=y) = mergeRek xs (y:ys) [x]
	| otherwise = mergeRek (x:xs) ys [y]

mergeRek [] (y:ys) (z:zs) = (z:zs) ++ (y:ys)
mergeRek (x:xs) [] (z:zs) = (z:zs) ++ (x:xs)
mergeRek (x:xs) (y:ys) (z:zs) 
	| (x<=y) = mergeRek xs (y:ys) (z:zs ++ [x])
	| otherwise = mergeRek (x:xs) ys (z:zs ++ [y])

mergeSort [] = []
mergeSort (x:xs) = 
	| length x:xs == 1 = x:xs
	| otherwise = merge mergeSort LINKS mergeSort RECHTS
	where left (x:xs) 
