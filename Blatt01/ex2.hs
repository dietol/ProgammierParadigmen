module Sort where

insert x [] = [x]
insert x (y:ys)
	| (x<y) = x:y:ys
	| otherwise = y:(insert x ys) 

insertSort xs = insertSortHelp xs []

insertSortHelp [] ys = ys
insertSortHelp (x:xs) ys = insertSortHelp xs (insert x ys)


merge [] ys = ys
merge (x:xs) ys = merge xs (insert x ys)

mergeSort [] = []
mergeSort xs 
	| (length xs == 1) = xs
	| otherwise =  merge (mergeSort (take ((length xs) `div` 2) xs)) (mergeSort (drop ((length xs) `div` 2) xs))

