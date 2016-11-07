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
mergeSort (x:xs)  
	| length (x:xs) == 1 = x:xs
	| otherwise = merge (mergeSort (leftDiv (x:xs))) (mergeSort (rightDiv (x:xs))) 

leftDiv [] = []
leftDiv (x:xs) = leftDivHelp xs [x] ((div (length (x:xs)) 2)- 1)

leftDivHelp (x:xs) (y:ys) a
	| a < 0 = error "rekursion error"
	| a == 0 = y:ys
	| otherwise = leftDivHelp xs (y:ys ++ [x]) (a-1)

rightDiv [] = []
rightDiv (x:xs) = reverse (rightDivHelp (reverse (init xs)) [last xs] ((div (length (x:xs)) 2) + (mod (length (x:xs)) 2)))

rightDivHelp (x:xs) (y:ys) a
        | a < 0 = error "rekursion error"
        | a == 1 = y:ys
        | otherwise = rightDivHelp xs (y:ys ++ [x]) (a-1)


